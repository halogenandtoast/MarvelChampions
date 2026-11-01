module Marvel.Encounter.AgeOfApocalypseFourHorsemenSpec (spec) where

import Control.Monad (replicateM_)
import Data.HashMap.Strict qualified as HashMap
import Marvel.Attachment.Cards qualified as Attachments
import Marvel.Attachment.Types qualified as Attachment
import Marvel.Hp (unHp)
import Marvel.Keyword
import Marvel.Matchers
import Marvel.Message.Lifted qualified as L
import Marvel.Minion.Cards qualified as Minions
import Marvel.Modifier
import Marvel.Phase
import Marvel.Query
import Marvel.Scenario.AgeOfApocalypseFourHorsemen
import Marvel.SideScheme.Cards qualified as Schemes
import Marvel.Trait (Trait (Aerial))
import Marvel.Stats (Sch (..))
import Marvel.Treachery.Cards qualified as Treacheries
import Marvel.Villain.Types qualified as Villain
import Marvel.Ability qualified as Ability
import TestImport

horsemen :: Difficulty -> IO TestGame
horsemen mode = do
  (application, trace, logs) <- dealtGameLogged (ScenarioSetup "45081a" [] mode) [starter "spider-man"] 1
  seating <- runGameApp application (getsGame gameSeats)
  let tg = TestGame application trace logs seating
  inGame tg toFirstTurn
  pure tg

lifted :: L.QueueT GameAppT () -> GameAppT ()
lifted action = resolve =<< L.runQueueT (L.capture action)

named :: Text -> GameAppT VillainId
named name = maybe (error "horseman missing") pure =<< findHorseman name

actualHp :: VillainId -> GameAppT Int
actualHp vid = getsGame $ maybe 0 (unHp . Villain.villainHp . toAttrs) . HashMap.lookup vid . gameVillains

villainCodes :: GameAppT [CardCode]
villainCodes = getsGame $ map (toCardCode . getCardDef) . HashMap.elems . gameVillains

attachmentIdOf :: EncounterCard -> AttachmentId
attachmentIdOf = AttachmentId . unCardId . ecCardId

attachmentHost :: EncounterCard -> GameAppT (Maybe EnemyId)
attachmentHost card = getsGame $ join . fmap (Attachment.attachmentEnemy . toAttrs) . HashMap.lookup (attachmentIdOf card) . gameAttachments

asHero :: IdentityId -> GameAppT ()
asHero p = changeForm p >> settleForced >> clearHand p

clearStartingSchemes :: GameAppT ()
clearStartingSchemes = do
  schemes <- getsGame (HashMap.keys . gameSideSchemes)
  for_ schemes $ \sid -> resolve [RemoveFromPlay $ SideSchemeRef sid]

-- Resolve mandatory ability windows while leaving real target/resource choices
-- for the test to answer explicitly. TestImport.pass only declines optional ones.
settleForced :: GameAppT ()
settleForced = do
  pass
  question <- asked
  case question >>= (headMay . filter forced . snd) of
    Just choice -> choose "resolve mandatory ability" (== choice) >> settleForced
    Nothing -> pure ()
 where
  forced = \case
    UseAbility ab -> abilityType ab `elem` [Ability.ForcedInterrupt, Ability.ForcedResponse, Ability.WhenRevealed]
    _ -> False


spec :: Spec
spec = describe "Age of Apocalypse Four Horsemen" do
  for_ [(Normal, ["45081a", "45082a", "45083a", "45084a"], 9), (Expert, ["45081b", "45082b", "45083b", "45084b"], 12)] $ \(mode, codes, hp) ->
    it ("reveals four correct villains and one random signature side scheme: " <> show mode) do
      tg <- horsemen mode
      inGame tg do
        actual <- villainCodes
        actual `shouldMatchList` codes
        ids <- selectList AnyVillain
        traverse actualHp ids `shouldReturn` replicate 4 hp
        HashMap.size <$> getsGame gameSideSchemes `shouldReturn` 1
        active <- selectOne ActiveVillain
        active `shouldSatisfy` isJust

  it "moves the counter cyclically after a named inactive villain activates" do
    tg <- horsemen Normal
    inGame tg do
      let p = player1 tg
      before <- selectJust ActiveVillain
      nonactive <- testHead . filter (/= before) <$> selectList AnyVillain
      lifted $ L.villainSchemesAgainst nonactive p
      after <- selectJust ActiveVillain
      (after == before) `shouldBe` False
      replicateM_ 3 $ lifted moveHorsemanCounter
      selectOne ActiveVillain `shouldReturn` Just before

  it "retains a villain at zero hit points while another is still standing" do
    tg <- horsemen Normal
    inGame tg do
      war <- named "War"
      resolve [VillainMessage war $ VillainDamaged GameRef $ toDamage 99 FromAbility]
      actualHp war `shouldReturn` 0
      length <$> villainCodes `shouldReturn` 4
      getModifiers (VillainRef war) >>= (`shouldSatisfy` elem CannotBeDefeated)

  it "wins when the final horseman reaches zero hit points" do
    tg <- horsemen Normal
    inGame tg do
      ids <- selectList AnyVillain
      for_ ids $ \vid -> resolve [VillainMessage vid $ VillainDamaged GameRef $ toDamage 99 FromAbility]
      getsGame gameState `shouldReturn` Finished Won

  it "Golden Horse finds a lowest-hit-point non-Aerial villain and keeps its effective hit points at one" do
    tg <- horsemen Normal
    inGame tg do
      let p = player1 tg
      clearStartingSchemes
      war <- named "War"
      resolve [VillainMessage war $ VillainDamaged GameRef $ toDamage 99 FromAbility]
      card <- revealNow p Attachments.goldenHorse
      chooseIf (isTargetChoice war)
      settleForced
      attachmentHost card `shouldReturn` Just (EnemyVillainId war)
      actualHp war `shouldReturn` 0
      horsemanHitPoints war `shouldReturn` 1
      enemyMatches (EnemyWithTrait Aerial) (EnemyVillainId war) `shouldReturn` True

  it "Metal Wings attaches to Death, moves the counter to him, and grants retaliate" do
    tg <- horsemen Normal
    inGame tg do
      death <- named "Death"
      card <- revealNow (player1 tg) Attachments.metalWings
      settleForced
      attachmentHost card `shouldReturn` Just (EnemyVillainId death)
      selectOne ActiveVillain `shouldReturn` Just death
      getModifiers (VillainRef death) >>= (`shouldSatisfy` elem (KeywordModifier $ Retaliate 1))

  it "War discards an upgrade or support after attacking" do
    tg <- horsemen Normal
    inGame tg do
      let p = player1 tg
      asHero p
      clearStartingSchemes
      card <- putIntoPlay p "01008"
      war <- named "War"
      _ <- setEncounterDeck [Treacheries.advance, Treacheries.advance] []
      runNow [VillainMessage war $ VillainAttacks p]
      settleForced
      choose "discard upgrade" (isTargetChoice $ upgradeIdOf card)
      settleForced
      length <$> upgradesWith "01008" `shouldReturn` 0

  it "War's zero-hit-point response does not discard a controlled card" do
    tg <- horsemen Normal
    inGame tg do
      let p = player1 tg
      asHero p
      clearStartingSchemes
      _ <- putIntoPlay p "01008"
      war <- named "War"
      resolve [VillainMessage war $ VillainDamaged GameRef $ toDamage 99 FromAbility]
      _ <- setEncounterDeck [Treacheries.advance, Treacheries.advance] []
      runNow [VillainMessage war $ VillainAttacks p]
      settleForced
      length <$> upgradesWith "01008" `shouldReturn` 1

  it "Famine discards ten cards after attacking" do
    tg <- horsemen Normal
    inGame tg do
      let p = player1 tg
      asHero p
      clearStartingSchemes
      _ <- stackDeck p $ replicate 12 "01088"
      before <- length . unDeck . playerIdentityDeck <$> playerAttrs p
      famine <- named "Famine"
      _ <- setEncounterDeck [Treacheries.advance, Treacheries.advance] []
      runNow [VillainMessage famine $ VillainAttacks p]
      settleForced
      length . unDeck . playerIdentityDeck <$> playerAttrs p `shouldReturn` before - 10

  it "Pestilence blanks the identity until the next villain phase begins" do
    tg <- horsemen Normal
    inGame tg do
      let p = player1 tg
      asHero p
      clearStartingSchemes
      pestilence <- named "Pestilence"
      _ <- setEncounterDeck [Treacheries.advance, Treacheries.advance] []
      runNow [VillainMessage pestilence $ VillainAttacks p]
      settleForced
      getModifiers (IdentityRef p) >>= (`shouldSatisfy` elem Blank)
      resolve [BeginPhase VillainPhase]
      getModifiers (IdentityRef p) >>= (`shouldSatisfy` notElem Blank)

  it "Death deals one nonattack damage to every controlled character after attacking" do
    tg <- horsemen Normal
    inGame tg do
      let p = player1 tg
      asHero p
      clearStartingSchemes
      _ <- putIntoPlay p "01002"
      death <- named "Death"
      _ <- setEncounterDeck [Treacheries.advance, Treacheries.advance] []
      runNow [VillainMessage death $ VillainAttacks p]
      settleForced
      damageOf p `shouldReturn` 3
      allyDamage <$> allyIn "01002" `shouldReturn` 1

  for_ [(Schemes.theRavagesOfWar, "45086" :: CardCode), (Schemes.aTimeOfFamine, "45087"), (Schemes.plagueAndPestilence, "45088"), (Schemes.theSpecterOfDeath, "45089")] $ \(def, code) ->
    it ("resolves the defeated side scheme's matching Horseman penalty: " <> show code) do
      tg <- horsemen Normal
      inGame tg do
        let p = player1 tg
        asHero p
        clearStartingSchemes
        upgrade <- putIntoPlay p "01008"
        _ <- putIntoPlay p "01002"
        _ <- stackDeck p $ replicate 12 "01088"
        before <- length . unDeck . playerIdentityDeck <$> playerAttrs p
        scheme <- sideSchemeOf <$> revealNow p def
        resolve [SideSchemeMessage scheme $ SideSchemeThreatRemoved GameRef 5]
        choose "thwart" (isHeroThwart p)
        choose "scheme" (isTargetChoice scheme)
        when (code == "45086") $ choose "discard upgrade" (isTargetChoice $ upgradeIdOf upgrade)
        settleForced
        case code of
          "45086" -> length <$> upgradesWith "01008" `shouldReturn` 0
          "45087" -> length . unDeck . playerIdentityDeck <$> playerAttrs p `shouldReturn` before - 10
          "45088" -> getModifiers (IdentityRef p) >>= (`shouldSatisfy` elem Blank)
          "45089" -> do
            damageOf p `shouldReturn` 1
            allyDamage <$> allyIn "01002" `shouldReturn` 1
          _ -> pure ()

  for_ [(Treacheries.horsemanOfWar, "War"), (Treacheries.horsemanOfFamine, "Famine"), (Treacheries.horsemanOfPestilence, "Pestilence"), (Treacheries.horsemanOfDeath, "Death")] $ \(def, name) ->
    it ("heals, toughens and activates the named Horseman: " <> show name) do
      tg <- horsemen Normal
      inGame tg do
        let p = player1 tg
        clearStartingSchemes
        vid <- named name
        resolve [VillainMessage vid $ VillainDamaged GameRef $ toDamage 4 FromAbility]
        before <- mainSchemeThreatNow
        _ <- setEncounterDeck [Treacheries.advance, Treacheries.advance] []
        _ <- revealNow p def
        settleForced
        actualHp vid `shouldReturn` 7
        attrs <- getsGame $ toAttrs . fromMaybe (error "missing named horseman") . HashMap.lookup vid . gameVillains
        Villain.villainTough attrs `shouldBe` True
        after <- mainSchemeThreatNow
        (after > before) `shouldBe` True

  it "Rough Riders resolves two responses even from zero-hit-point Horsemen, moving the counter only once" do
    tg <- horsemen Normal
    inGame tg do
      let p = player1 tg
      asHero p
      clearStartingSchemes
      famine <- named "Famine"
      resolve [VillainMessage famine $ VillainDamaged GameRef $ toDamage 99 FromAbility, SetActiveVillain famine]
      _ <- stackDeck p $ replicate 12 "01088"
      before <- length . unDeck . playerIdentityDeck <$> playerAttrs p
      _ <- revealNow p Treacheries.roughRiders
      settleForced
      after <- length . unDeck . playerIdentityDeck <$> playerAttrs p
      (after <= before - 10) `shouldBe` True
      active <- selectJust ActiveVillain
      (active == famine) `shouldBe` False

  describe "Hounds modular set" do
    it "Ahab finds Release the Hounds, or places three threat there when already present" do
      tg <- testGame ["spider-man"]
      inGame tg do
        let p = player1 tg
        _ <- setEncounterDeck [Schemes.releaseTheHounds, Treacheries.advance] []
        _ <- revealNow p Minions.ahab
        settleForced
        sid <- selectJust $ SideSchemeIs Schemes.releaseTheHounds
        sideSchemeThreat sid `shouldReturn` 5
        ahab <- selectJust $ MinionIs Minions.ahab
        resolve [RemoveFromPlay $ MinionRef ahab]
        _ <- revealNow p Minions.ahab
        settleForced
        sideSchemeThreat sid `shouldReturn` 8

    it "Hound changes an alter ego to hero form" do
      tg <- testGame ["spider-man"]
      inGame tg do
        let p = player1 tg
        _ <- revealNow p Minions.hound
        settleForced
        identityMatches HeroIdentity p `shouldReturn` True

    it "Hound attacks a hero when revealed" do
      tg <- testGame ["spider-man"]
      inGame tg do
        let p = player1 tg
        asHero p
        _ <- revealNow p Minions.hound
        settleForced
        damageOf p `shouldReturn` 2

    it "Ahab's Energy Spear attaches to Ahab before the villain and grants overkill and piercing" do
      tg <- testGame ["spider-man"]
      inGame tg do
        let p = player1 tg
        ahab <- addMinion p Minions.ahab
        card <- revealNow p Attachments.ahabsEnergySpear
        settleForced
        attachmentHost card `shouldReturn` Just (EnemyMinionId ahab)
        modifiers <- getModifiers (MinionRef ahab)
        KeywordModifier Overkill `elem` modifiers `shouldBe` True
        KeywordModifier Piercing `elem` modifiers `shouldBe` True
        runNow [MinionMessage ahab $ MinionAttacks p]
        settleForced
        exists <- getsGame $ HashMap.member (attachmentIdOf card) . gameAttachments
        exists `shouldBe` False

    it "Release the Hounds deals the defeating player a facedown Hound" do
      tg <- testGame ["spider-man"]
      inGame tg do
        let p = player1 tg
        asHero p
        side <- sideSchemeOf <$> revealNow p Schemes.releaseTheHounds
        _ <- setEncounterDeck [Minions.hound, Treacheries.advance] []
        resolve [SideSchemeMessage side $ SideSchemeThreatRemoved GameRef 4]
        choose "thwart" (isHeroThwart p)
        choose "Release the Hounds" (isTargetChoice side)
        settleForced
        map toCardCode <$> dealtTo p `shouldReturn` ["45098"]

  for_ [(Treacheries.horsemanOfWar, "War"), (Treacheries.horsemanOfFamine, "Famine"), (Treacheries.horsemanOfPestilence, "Pestilence"), (Treacheries.horsemanOfDeath, "Death")] $ \(def, name) ->
    it ("the boost waits for this activation then activates " <> show name <> " without a boost") do
      tg <- horsemen Normal
      inGame tg do
        let p = player1 tg
        clearStartingSchemes
        active <- selectJust ActiveVillain
        target <- named name
        original <- getsGame $ toAttrs . fromMaybe (error "missing active villain") . HashMap.lookup active . gameVillains
        namedAttrs <- getsGame $ toAttrs . fromMaybe (error "missing named villain") . HashMap.lookup target . gameVillains
        let scheme = unSch (Villain.villainScheme original) + unSch (Villain.villainScheme namedAttrs)
        before <- mainSchemeThreatNow
        _ <- setEncounterDeck [def, Schemes.theRavagesOfWar] []
        lifted $ L.villainSchemesAgainst active p
        mainSchemeThreatNow `shouldReturn` before + scheme
        map toCardCode <$> scenarioEncounterDeckNow `shouldReturn` ["45086"]

  it "Release the Hounds gives only the first Hound revealed this phase surge" do
    tg <- testGame ["spider-man"]
    inGame tg do
      let p = player1 tg
      _ <- revealNow p Schemes.releaseTheHounds
      _ <- setEncounterDeck [Minions.hound, Treacheries.advance] []
      _ <- revealNow p Minions.hound
      settleForced
      -- Surge deals a facedown card; the encounter phase then reveals it.
      map toCardCode <$> dealtTo p `shouldReturn` ["45098"]
      map toCardCode <$> scenarioEncounterDeckNow `shouldReturn` ["01186"]
      runNow [IdentityMessage p RevealEncounterCards]
      settleForced
      -- The second Hound attacks in hero form, without gaining surge again.
      dealtTo p `shouldReturn` []
      damageOf p `shouldReturn` 2
      map toCardCode <$> scenarioEncounterDeckNow `shouldReturn` ["01186"]

  it "Metal Wings and Golden Horse both preserve zero-HP villains until discarded" do
    tg <- horsemen Normal
    inGame tg do
      let p = player1 tg
      clearStartingSchemes
      death <- named "Death"
      card <- revealNow p Attachments.metalWings
      settleForced
      resolve [VillainMessage death $ VillainDamaged GameRef $ toDamage 99 FromAbility]
      actualHp death `shouldReturn` 0
      horsemanHitPoints death `shouldReturn` 1
      others <- filter (/= death) <$> selectList AnyVillain
      for_ others $ \vid -> resolve [VillainMessage vid $ VillainDamaged GameRef $ toDamage 99 FromAbility]
      getsGame gameState `shouldReturn` InProgress
      resolve [RemoveFromPlay $ AttachmentRef $ attachmentIdOf card]
      lifted $ L.cardSpecific GameRef "aoaHorsemenCheckVictory" ()
      getsGame gameState `shouldReturn` Finished Won

  it "Famine's attack against an ally still discards its controller's top ten cards" do
    tg <- horsemen Normal
    inGame tg do
      let p = player1 tg
      asHero p
      clearStartingSchemes
      ally <- putIntoPlay p "01002"
      _ <- stackDeck p $ replicate 12 "01088"
      before <- length . unDeck . playerIdentityDeck <$> playerAttrs p
      famine <- named "Famine"
      _ <- setEncounterDeck [Treacheries.advance, Treacheries.advance] []
      runNow [VillainMessage famine $ VillainAttacksAlly p $ allyIdOf ally]
      settleForced
      length . unDeck . playerIdentityDeck <$> playerAttrs p `shouldReturn` before - 10
