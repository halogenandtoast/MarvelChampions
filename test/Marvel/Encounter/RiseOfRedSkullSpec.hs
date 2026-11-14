-- | The Rise of Red Skull's Crossbones and Absorbing Man scenarios: the cards that ask
-- something new of the engine.
module Marvel.Encounter.RiseOfRedSkullSpec (spec) where

import Data.HashMap.Strict qualified as HashMap
import Data.List qualified as List
import Marvel.Area (Pile (..), experimentalWeaponsDeck)
import Marvel.Attachment.Cards qualified as AttachmentCards
import Marvel.Attachment.Types (Attrs (..))
import Marvel.Choice qualified as Choice
import Marvel.EncounterSet qualified as EncounterSet
import Marvel.Environment.Cards qualified as EnvironmentCards
import Marvel.Keyword (Keyword (Piercing))
import Marvel.Matchers (EnvironmentMatcher (AnyEnvironment), VillainMatcher (VillainWithTrait))
import Marvel.Query (selectAny, selectList)
import Marvel.Trait (Trait (..))
import Data.Aeson.KeyMap qualified as KeyMap
import Data.Aeson.Types (parseEither)
import Marvel.Attack (attack)
import Marvel.Villain.Types (Villain (..), attackingL, getModifiedKeywords, overVillainAttrs)
import TestImport

-- | Answer what is asked, taking no defender, until the turn comes back.
undefended :: GameAppT ()
undefended = go (50 :: Int)
 where
  go 0 = pure ()
  go n =
    asked >>= \case
      Just (_, cs) | EndTurn `notElem` cs -> do
        if any isNoDefender cs then choose "no defender" isNoDefender else choose "first" (const True)
        go (n - 1)
      _ -> pure ()

spec :: Spec
spec = describe "The Rise of Red Skull" do
  it "Attack on Mount Athena makes the Experimental Weapons deck, 1B reveals its top card; its cards discard to the encounter discard pile" do
    tg <- testGameAgainst "04058" ["spider-man"]
    let you = player1 tg
    inGame tg do
      vid <- villainIdNow
      let onCrossbones = getsGame (filter ((== Just (EnemyVillainId vid)) . attachmentEnemy) . map toAttrs . HashMap.elems . gameAttachments)
      -- 1B's When Revealed has already put one Experimental Weapon on Crossbones
      Pile weapons _ <- getPile experimentalWeaponsDeck
      deck <- scenarioEncounterDeckNow
      let experimental = (== Just EncounterSet.ExperimentalWeapons) . cdEncounterSet . getCardDef
      atSetup <- onCrossbones
      liftIO $ (length weapons, any experimental deck, map (cdEncounterSet . attachmentCardDef) atSetup)
        `shouldBe` (3, False, [Just EncounterSet.ExperimentalWeapons])
      resolve [UsingDeck experimentalWeaponsDeck (DrawAndRevealEncounterCard you)]
      attached <- onCrossbones
      Pile left _ <- getPile experimentalWeaponsDeck
      liftIO $ (length left, map (cdEncounterSet . attachmentCardDef) attached) `shouldBe` (2, replicate 2 (Just EncounterSet.ExperimentalWeapons))
      for_ attached \a -> resolve [RemoveFromPlay (AttachmentRef $ attachmentId a)]
      discard <- encounterDiscardCodes
      Pile _ weaponDiscard <- getPile experimentalWeaponsDeck
      liftIO $ (List.sort (map (toCardCode . attachmentCardDef) attached), weaponDiscard) `shouldBe` (List.sort (take 2 discard), [])

  it "Crossbones' Armor takes the damage Crossbones would take, and is discarded with 5 or more" do
    tg <- testGameAgainst "04058" ["spider-man"]
    let you = player1 tg
    inGame tg do
      vid <- villainIdNow
      _ <- revealNow you AttachmentCards.crossbonesArmor
      let hit n = resolve =<< Choice.choiceMessages you (DamageEnemy (EnemyRef $ EnemyVillainId vid) (toSource you) (toDamage n FromAbility))
          armor = getsGame (map toAttrs . filter ((== "04065") . toCardCode . attachmentCardDef . toAttrs) . HashMap.elems . gameAttachments)
      hit 3
      first <- map attachmentDamage <$> armor
      villainDamageNow `shouldReturn` 0
      hit 2
      second <- armor
      villainDamageNow `shouldReturn` 0
      liftIO $ (first, length second) `shouldBe` ([3], 0)
      hit 1
      villainDamageNow `shouldReturn` 1

  it "Crossbones' attacks with a Weapon attachment gain piercing, discarding the hero's tough status card" do
    tg <- testGameAgainst "04058" ["spider-man"]
    let you = player1 tg
    inGame tg do
      changeForm you
      vid <- villainIdNow
      -- the Experimental Weapon 1B gave him is a Weapon too: start unarmed
      starting <- getsGame (filter ((== Just (EnemyVillainId vid)) . attachmentEnemy) . map toAttrs . HashMap.elems . gameAttachments)
      for_ starting \a -> resolve [RemoveFromPlay (AttachmentRef $ attachmentId a)]
      unarmed <- getModifiedKeywords . toAttrs =<< getVillain
      _ <- revealNow you AttachmentCards.combatKnife
      armed <- getModifiedKeywords . toAttrs =<< getVillain
      liftIO $ (Piercing `elem` unarmed, Piercing `elem` armed) `shouldBe` (False, True)
      resolve [IdentityMessage you IdentityBecomeTough]
      before <- damageOf you
      start [VillainMessage vid (VillainAttacks you)]
      undefended
      tough <- (.playerIdentityTough) <$> playerAttrs you
      after <- damageOf you
      liftIO $ (tough, after > before) `shouldBe` (False, True)

  it "Crossbones' Machine Gun, a forced interrupt, resolves before Spider-Sense on the same attack" do
    tg <- testGameAgainst "04058" ["spider-man"]
    let you = player1 tg
    inGame tg do
      changeForm you
      vid <- villainIdNow
      -- only the gun on him: the Experimental Weapon 1B gave him goes
      starting <- getsGame (filter ((== Just (EnemyVillainId vid)) . attachmentEnemy) . map toAttrs . HashMap.elems . gameAttachments)
      for_ starting \a -> resolve [RemoveFromPlay (AttachmentRef $ attachmentId a)]
      _ <- revealNow you AttachmentCards.crossbonesMachineGun
      let gun = getsGame (map toAttrs . filter ((== "04064") . toCardCode . attachmentCardDef . toAttrs) . HashMap.elems . gameAttachments)
          -- answer what comes between the gun and Spider-Sense, until Spider-Sense is offered
          untilSpiderSense :: Int -> GameAppT Bool
          untilSpiderSense 0 = pure False
          untilSpiderSense n = asked >>= \case
            Just (_, cs)
              | any (isAbilityOf you) cs -> pure True
              | EndTurn `notElem` cs -> choose "first" (const True) >> untilSpiderSense (n - 1)
            _ -> pure False
      [g] <- gun
      start [VillainMessage vid (VillainAttacks you)]
      Just (_, first) <- asked
      liftIO $ (any (isAbilityOf (AttachmentRef $ attachmentId g)) first, any (isAbilityOf you) first) `shouldBe` (True, False)
      choose "Machine Gun" (isAbilityOf (AttachmentRef $ attachmentId g))
      offered <- untilSpiderSense 10
      after <- map attachmentCounters <$> gun
      liftIO $ (offered, after) `shouldBe` (True, [attachmentCounters g - 1])

  it "None Shall Pass keeps one environment in play, Absorbing Man has its trait, and a delay counter follows step one" do
    tg <- testGameAgainst "04076" ["spider-man"]
    let you = player1 tg
    inGame tg do
      [_] <- selectList AnyEnvironment
      -- reveal an environment of a trait other than the one in play
      lost <- getsGame (fromMaybe (error "no environment") . headMay . concatMap (toList . cdTraits . getCardDef . toAttrs) . HashMap.elems . gameEnvironments)
      hadLost <- selectAny (VillainWithTrait lost)
      let (next, gained) = if lost == Ice then (EnvironmentCards.denseForest, Wood) else (EnvironmentCards.snowyHillside, Ice)
      dealt <- length <$> dealtTo you
      _ <- revealNow you next
      now <- getsGame (map (toCardCode . getCardDef . toAttrs) . HashMap.elems . gameEnvironments)
      hasGained <- selectAny (VillainWithTrait gained)
      hasLost <- selectAny (VillainWithTrait lost)
      surged <- length <$> dealtTo you
      liftIO $ (hadLost, now, hasGained, hasLost, surged) `shouldBe` (True, [toCardCode next], True, False, dealt + 1)
      resolve [AccelerateMainScheme]
      getYourMainSchemeCounters `shouldReturn` 1

  it "Absorbing Man gains whatever trait an environment has" do
    tg <- testGameAgainst "04076" ["spider-man"]
    inGame tg do
      -- the environment in play given a trait none of his scenario's have
      withGame_ $ entitiesL . environmentsL . each %~ \e -> case toJSON e of
        Object o
          | Just (Object def) <- KeyMap.lookup "environmentCardDef" o ->
              either error id $ parseEither parseJSON $ Object $ KeyMap.insert "environmentCardDef" (Object $ KeyMap.insert "cdTraits" (toJSON [Elite]) def) o
        _ -> error "no environment"
      selectAny (VillainWithTrait Elite) `shouldReturn` True

  it "an attack keeps a keyword it gained as it was declared, in the would window, when it begins" do
    tg <- testGame ["spider-man"]
    let you = player1 tg
    inGame tg do
      changeForm you
      vid <- villainIdNow
      resolve [IdentityMessage you IdentityBecomeTough]
      before <- damageOf you
      -- the attack declared, as Rhino attacking has it before his would windows, and
      -- piercing gained there
      withGame_ $ entitiesL . villainsL . ix vid %~ \(Villain v) -> Villain $ overVillainAttrs (attackingL ?~ attack (EnemyVillainId vid) (IdentityCharacter you) 0) v
      start [VillainMessage vid (VillainAttackGains Piercing), VillainMessage vid (VillainBeginAttack you), VillainMessage vid VillainEndAttack]
      undefended
      tough <- (.playerIdentityTough) <$> playerAttrs you
      after <- damageOf you
      liftIO $ (tough, after > before) `shouldBe` (False, True)
