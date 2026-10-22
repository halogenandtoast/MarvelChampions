-- | The Rise of Red Skull's Crossbones and Absorbing Man scenarios: the cards that ask
-- something new of the engine.
module Marvel.Encounter.RiseOfRedSkullSpec (spec) where

import Data.HashMap.Strict qualified as HashMap
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
import Marvel.Villain.Types (getModifiedKeywords)
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
  it "Attack on Mount Athena makes the Experimental Weapons deck; its cards discard to the encounter discard pile" do
    tg <- testGameAgainst "04058" ["spider-man"]
    let you = player1 tg
    inGame tg do
      Pile weapons _ <- getPile experimentalWeaponsDeck
      deck <- scenarioEncounterDeckNow
      let experimental = (== Just EncounterSet.ExperimentalWeapons) . cdEncounterSet . getCardDef
      liftIO $ (length weapons, any experimental deck) `shouldBe` (4, False)
      resolve [UsingDeck experimentalWeaponsDeck (DrawAndRevealEncounterCard you)]
      vid <- villainIdNow
      attached <- getsGame (filter ((== Just (EnemyVillainId vid)) . attachmentEnemy) . map toAttrs . HashMap.elems . gameAttachments)
      Pile left _ <- getPile experimentalWeaponsDeck
      liftIO $ (length left, map (cdEncounterSet . attachmentCardDef) attached) `shouldBe` (3, [Just EncounterSet.ExperimentalWeapons])
      for_ attached \a -> resolve [RemoveFromPlay (AttachmentRef $ attachmentId a)]
      discard <- encounterDiscardCodes
      Pile _ weaponDiscard <- getPile experimentalWeaponsDeck
      liftIO $ (map (toCardCode . attachmentCardDef) attached, weaponDiscard) `shouldBe` (take 1 discard, [])

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
      getMainSchemeCounters `shouldReturn` 1
