module Marvel.Encounter.TheHoodSpec (spec) where

import Data.HashMap.Strict qualified as HashMap
import Data.List (nub)
import Data.Maybe (fromJust)
import Marvel.Attachment.Cards qualified as Attachments
import Marvel.Damage
import Marvel.EncounterSet qualified as Set
import Marvel.Keyword
import Marvel.Phase
import Marvel.Matchers
import Marvel.Message.Lifted qualified as Lifted
import Marvel.Minion.Cards qualified as Minions
import Marvel.Query
import Marvel.Scenario.Types (Scenario (..), Attrs (scenarioSetAsideCards, scenarioEncounterSets))
import Marvel.SideScheme.Cards qualified as SideSchemes
import Marvel.Treachery.Cards qualified as Treacheries
import Marvel.Villain.Types qualified as Villain
import Marvel.Villain.Villains.TheHood (foulPlay, recruitModular)
import TestImport

hood :: [Text] -> IO TestGame
hood = testGameAgainst "24001"

hoodExpert :: IO TestGame
hoodExpert = do
  let setup = withTestModulars (fromJust $ recommendedSetup "24001") {difficulty = Expert}
  (app, ran, logged) <- dealtGameLogged setup [starter "spider-man"] 1
  seats <- runGameApp app (getsGame gameSeats)
  let tg = TestGame app ran logged seats
  inGame tg toFirstTurn
  pure tg

asideSets :: GameAppT [Set.EncounterSet]
asideSets = getsGame (nub . filter (`elem` Set.modularSets) . catMaybes . map (cdEncounterSet . getCardDef) . scenarioSetAsideCards . toAttrs . gameScenario)

stage :: GameAppT Natural
stage = getVillainStage =<< villainIdNow

advance :: GameAppT ()
advance = resolve . pure . (\v -> VillainMessage v VillainDefeated) =<< villainIdNow

spec :: Spec
spec = describe "The Hood" do
  it "has no recommended modular sets and does not silently choose any" do
    let setup = fromJust $ recommendedSetup "24001"
        sc = fromJust $ lookupScenario setup
    modulars setup `shouldBe` []
    let chosen = filter (`elem` Set.modularSets) (toList $ scenarioEncounterSets $ toAttrs sc)
    (chosen `shouldBe` [] :: IO ())

  it "uses exactly the selected modular sets without adding defaults" do
    let setup = ScenarioSetup "24001" [Set.BombScare] Normal
        sc = fromJust $ lookupScenario setup
        chosen = filter (`elem` Set.modularSets) (toList $ scenarioEncounterSets $ toAttrs sc)
    (chosen `shouldBe` [Set.BombScare] :: IO ())

  it "sets aside six whole modular sets after recruiting one, with stage I and Making Connections" do
    tg <- hood ["spider-man"]
    inGame tg do
      stage `shouldReturn` 1
      length <$> asideSets `shouldReturn` 6
      schemes <- catMaybes <$> (traverse getMainSchemeCode =<< getsGame (HashMap.keys . gameMainSchemes))
      liftIO $ schemes `shouldBe` ["24004"]
      mainSchemeThreatNow `shouldReturn` 1

  it "recruits a whole set and removes it from the set-aside area" do
    tg <- hood ["spider-man"]
    inGame tg do
      before <- asideSets
      resolve =<< Lifted.runQueueT (Lifted.capture recruitModular)
      after <- asideSets
      liftIO $ length after `shouldBe` 5
      let recruited = filter (`notElem` after) before
      deckSets <- map (cdEncounterSet . getCardDef) <$> scenarioEncounterDeckNow
      liftIO $ all ((`elem` deckSets) . Just) recruited `shouldBe` True

  it "Foul Play I discards a Hood card without dealing it" do
    tg <- hood ["spider-man"]
    inGame tg do
      let you = player1 tg
      before <- length <$> dealtTo you
      void $ setEncounterDeck [Treacheries.upperHand, Treacheries.advance] []
      resolve =<< Lifted.runQueueT (Lifted.capture $ foulPlay you)
      length <$> dealtTo you `shouldReturn` before
      map toCardCode <$> encounterDiscard `shouldReturn` ["24013"]

  it "Foul Play I deals the actual discarded non-Hood card" do
    tg <- hood ["spider-man"]
    inGame tg do
      let you = player1 tg
      ([card, _], _) <- setEncounterDeck [Treacheries.advance, Treacheries.upperHand] []
      resolve =<< Lifted.runQueueT (Lifted.capture $ foulPlay you)
      dealt <- dealtTo you
      liftIO $ card `elem` dealt `shouldBe` True
      discard <- encounterDiscard
      liftIO $ card `elem` discard `shouldBe` False

  it "stage II recruits another set and deals only the first eligible discard" do
    tg <- hood ["spider-man"]
    inGame tg do
      advance
      stage `shouldReturn` 2
      length <$> asideSets `shouldReturn` 5
      let you = player1 tg
      ([first, second, _], _) <- setEncounterDeck [Treacheries.advance, Treacheries.assault, Treacheries.upperHand] []
      resolve =<< Lifted.runQueueT (Lifted.capture $ foulPlay you)
      dealt <- dealtTo you
      liftIO $ first `elem` dealt `shouldBe` True
      liftIO $ second `elem` dealt `shouldBe` False
      discard <- encounterDiscard
      liftIO $ second `elem` discard `shouldBe` True

  it "stage II skips a Hood card to deal the second discard" do
    tg <- hood ["spider-man"]
    inGame tg do
      advance
      let you = player1 tg
      ([_, card, _], _) <- setEncounterDeck [Treacheries.upperHand, Treacheries.advance, Treacheries.upperHand] []
      resolve =<< Lifted.runQueueT (Lifted.capture $ foulPlay you)
      dealt <- dealtTo you
      liftIO $ card `elem` dealt `shouldBe` True

  it "Promised Prosperity recruits, adds acceleration, and penalizes a player dealt no card" do
    tg <- hood ["spider-man"]
    inGame tg do
      -- No sets remain: leave a controlled deck for the scheme's Foul Play.
      withGame_ \g -> g {gameScenario = Scenario $ TheHoodScenario (toAttrs (gameScenario g)) {scenarioSetAsideCards = []}}
      void $ setEncounterDeck (replicate 10 Treacheries.upperHand) []
      resolve . pure . NextMainScheme =<< mainSchemeIdNow
      mainSchemeThreatNow `shouldReturn` 4
      accelerationTokensNow `shouldReturn` 1

  it "Crime State adds another token and resolves Foul Play after acceleration" do
    tg <- hood ["spider-man"]
    inGame tg do
      resolve . pure . NextMainScheme =<< mainSchemeIdNow
      resolve . pure . NextMainScheme =<< mainSchemeIdNow
      accelerationTokensNow `shouldReturn` 2
      let you = player1 tg
      before <- length <$> dealtTo you
      void $ setEncounterDeck [Treacheries.advance, Treacheries.upperHand] []
      resolve [AccelerateMainScheme]
      length <$> dealtTo you `shouldReturn` (before + 1)

  it "the Hood's Pistol adds ATK and SCH" do
    tg <- hood ["spider-man"]
    inGame tg do
      void $ revealNow (player1 tg) Attachments.theHoodsPistol
      vid <- villainIdNow
      getVillainAttackValue vid `shouldReturn` 2
      getVillainSchemeValue vid `shouldReturn` 2

  it "the Hood's Mantle grants retaliate and requires two confuse cards to cancel a scheme" do
    tg <- hood ["spider-man"]
    inGame tg do
      void $ revealNow (player1 tg) Attachments.theHoodsMantle
      vid <- villainIdNow
      v <- getsGame (toAttrs . fromJust . HashMap.lookup vid . gameVillains)
      keys <- Villain.getModifiedKeywords v
      liftIO $ all (`elem` keys) [Steady, Retaliate 1] `shouldBe` True
      resolve [GiveStatus (VillainRef vid) ConfusedStatus]
      selectAny (EnemyWithId (EnemyVillainId vid) <> ConfusedEnemy) `shouldReturn` False
      resolve [GiveStatus (VillainRef vid) ConfusedStatus]
      selectAny (EnemyWithId (EnemyVillainId vid) <> ConfusedEnemy) `shouldReturn` True
      before <- mainSchemeThreatNow
      resolve [VillainMessage vid VillainSchemes]
      mainSchemeThreatNow `shouldReturn` before
      v' <- getsGame (toAttrs . fromJust . HashMap.lookup vid . gameVillains)
      liftIO $ Villain.villainConfused v' `shouldBe` False
      liftIO $ Villain.villainAdditionalConfuse v' `shouldBe` False

  it "Madame Masque resolves Foul Play both when revealed and when defeated" do
    tg <- hood ["spider-man"]
    inGame tg do
      let you = player1 tg
      before <- length <$> dealtTo you
      void $ setEncounterDeck [Treacheries.advance, Treacheries.assault, Treacheries.upperHand] []
      c <- revealNow you Minions.madameMasque
      length <$> dealtTo you `shouldReturn` (before + 1)
      resolve [MinionMessage (minionOf c) (MinionDamaged (IdentityRef you) (toDamage 4 FromAbility))]
      length <$> dealtTo you `shouldReturn` (before + 2)

  it "Unbridled Ambition enters with its flat base plus hinder threat" do
    tg <- hood ["spider-man", "she-hulk"]
    inGame tg do
      c <- revealNow (player1 tg) SideSchemes.unbridledAmbition
      sideSchemeThreat (sideSchemeOf c) `shouldReturn` 6

  it "Field Recruitment recruits, resolves Foul Play, and leaves the game" do
    tg <- hood ["spider-man"]
    inGame tg do
      c <- revealNow (player1 tg) Treacheries.fieldRecruitment
      length <$> asideSets `shouldReturn` 5
      deck <- scenarioEncounterDeckNow
      discard <- encounterDiscard
      liftIO $ c `elem` (deck <> discard) `shouldBe` False

  it "Upper Hand activates the Hood before Foul Play" do
    tg <- hood ["spider-man"]
    inGame tg do
      let you = player1 tg
      before <- length <$> dealtTo you
      void $ setEncounterDeck [Treacheries.upperHand, Treacheries.advance, Treacheries.upperHand] []
      c <- encounterCard Treacheries.upperHand
      resolve [RevealEncounterCard you c]
      length <$> dealtTo you `shouldReturn` (before + 1)

  it "expert starts at stage II and stage III recruits and deals both eligible cards" do
    tg <- hoodExpert
    inGame tg do
      stage `shouldReturn` 2
      length <$> asideSets `shouldReturn` 5
      advance
      stage `shouldReturn` 3
      length <$> asideSets `shouldReturn` 4
      let you = player1 tg
      ([first, second, _], _) <- setEncounterDeck [Treacheries.advance, Treacheries.assault, Treacheries.upperHand] []
      resolve =<< Lifted.runQueueT (Lifted.capture $ foulPlay you)
      dealt <- dealtTo you
      liftIO $ all (`elem` dealt) [first, second] `shouldBe` True

  it "steady consumes both stun cards when they cancel an attack" do
    tg <- hood ["spider-man"]
    inGame tg do
      void $ revealNow (player1 tg) Attachments.theHoodsMantle
      vid <- villainIdNow
      resolve [GiveStatus (VillainRef vid) StunnedStatus, GiveStatus (VillainRef vid) StunnedStatus]
      before <- damageOf (player1 tg)
      resolve [VillainMessage vid (VillainAttacks $ player1 tg)]
      damageOf (player1 tg) `shouldReturn` before
      v <- getsGame (toAttrs . fromJust . HashMap.lookup vid . gameVillains)
      liftIO $ (Villain.villainStunned v, Villain.villainAdditionalStun v) `shouldBe` (False, False)

  it "removing the Mantle makes a single confusion cancel the next scheme" do
    tg <- hood ["spider-man"]
    inGame tg do
      c <- revealNow (player1 tg) Attachments.theHoodsMantle
      vid <- villainIdNow
      resolve [GiveStatus (VillainRef vid) ConfusedStatus]
      resolve [RemoveFromPlay (AttachmentRef $ AttachmentId $ unCardId $ ecCardId c)]
      before <- mainSchemeThreatNow
      resolve [VillainMessage vid VillainSchemes]
      mainSchemeThreatNow `shouldReturn` before

  it "Foul Play stops at the end of the old deck" do
    tg <- hoodExpert
    inGame tg do
      let you = player1 tg
      before <- length <$> dealtTo you
      void $ setEncounterDeck [Treacheries.upperHand] [Treacheries.advance]
      resolve =<< Lifted.runQueueT (Lifted.capture $ foulPlay you)
      length <$> dealtTo you `shouldReturn` before
      accelerationTokensNow `shouldReturn` 1

  it "Established Dominance resolves Foul Play after the Hood schemes" do
    tg <- hood ["spider-man"]
    inGame tg do
      let you = player1 tg
      void $ revealNow you Attachments.establishedDominance
      changeForm you
      before <- length <$> dealtTo you
      void $ setEncounterDeck [Treacheries.upperHand, Treacheries.advance, Treacheries.upperHand] []
      vid <- villainIdNow
      resolve [VillainMessage vid (VillainSchemesAgainst you)]
      length <$> dealtTo you `shouldReturn` (before + 1)

  it "Established Dominance's alter-ego action exhausts the identity and places threat to discard it" do
    tg <- hood ["spider-man"]
    inGame tg do
      let you = player1 tg
      c <- revealNow you Attachments.establishedDominance
      let target = AttachmentRef $ AttachmentId $ unCardId $ ecCardId c
      before <- mainSchemeThreatNow
      offers (isAbilityOf target) `shouldReturn` True
      choose "discard Established Dominance" (isAbilityOf target)
      isExhaustedNow you `shouldReturn` True
      mainSchemeThreatNow `shouldReturn` (before + 2)
      offers (isAbilityOf target) `shouldReturn` False

  it "defeating the last villain stage wins" do
    tg <- hood ["spider-man"]
    inGame tg do
      advance
      advance
      getsGame gameState `shouldReturn` Finished Won

  it "completing Crime State loses" do
    tg <- hood ["spider-man"]
    inGame tg do
      resolve . pure . NextMainScheme =<< mainSchemeIdNow
      resolve . pure . NextMainScheme =<< mainSchemeIdNow
      mid <- mainSchemeIdNow
      resolve [AdvanceMainScheme mid]
      getsGame gameState `shouldReturn` Finished Lost


  it "Unbridled Ambition resolves Foul Play for each player as the villain phase begins" do
    tg <- hood ["spider-man", "she-hulk"]
    inGame tg do
      let you = player1 tg
          them = player2 tg
      void $ revealNow you SideSchemes.unbridledAmbition
      before <- length <$> dealtTo you
      beforeThem <- length <$> dealtTo them
      ([first, second, _], _) <- setEncounterDeck [Treacheries.advance, Treacheries.assault, Treacheries.upperHand] []
      resolve [BeginPhase VillainPhase]
      dealt <- dealtTo you
      dealtThem <- dealtTo them
      liftIO $ first `elem` dealt `shouldBe` True
      liftIO $ second `elem` dealtThem `shouldBe` True
      liftIO $ length dealt `shouldBe` (before + 1)
      liftIO $ length dealtThem `shouldBe` (beforeThem + 1)

  it "Upper Hand and Established Dominance have no boost abilities" do
    tg <- hood ["spider-man"]
    inGame tg do
      let you = player1 tg
      changeForm you
      before <- length <$> dealtTo you
      void $ setEncounterDeck [Treacheries.upperHand, Attachments.establishedDominance, Treacheries.advance, Treacheries.upperHand] []
      vid <- villainIdNow
      resolve [VillainMessage vid (VillainSchemesAgainst you)]
      mid <- mainSchemeIdNow
      resolve [MainSchemeMessage mid (MainSchemeThreatRemoved GameRef 10)]
      resolve [VillainMessage vid (VillainSchemesAgainst you)]
      length <$> dealtTo you `shouldReturn` before
      getsGame (length . gameAttachments) `shouldReturn` 0
