module Marvel.Encounter.MojoOpeningSpec (spec) where

import Data.HashMap.Strict qualified as HashMap
import Data.Text qualified as Text
import Data.Maybe (fromJust)
import Marvel.Area (Pile (..))
import Marvel.Window qualified as W
import Marvel.Attachment.Cards qualified as Attachments
import Marvel.Damage
import Marvel.EncounterSet qualified as Set
import Marvel.Environment.Cards qualified as Environments
import Marvel.Keyword qualified as Keyword
import Marvel.Matchers
import Marvel.Message.Lifted qualified as Lifted
import Marvel.Minion.Cards qualified as Minions
import Marvel.Modifier
import Marvel.MojoOpening
import Marvel.Query
import Marvel.SideScheme.Cards qualified as Sides
import Marvel.SideScheme.Types qualified as Side
import Marvel.Treachery.Cards qualified as Treacheries
import TestImport

opening :: CardCode -> [Text] -> IO TestGame
opening code heroes = do
  let setup = (fromJust $ recommendedSetup code) {modulars = if code == "39001a" then [Set.Fantasy] else [Set.Fantasy, Set.Horror, Set.SciFi]}
  (app, ran, logged) <- dealtGameLogged setup (map starter heroes) 1
  seats <- runGameApp app (getsGame gameSeats)
  let tg = TestGame app ran logged seats
  inGame tg toFirstTurn
  pure tg

lifted :: Lifted.QueueT GameAppT () -> GameAppT ()
lifted action = resolve =<< Lifted.runQueueT (Lifted.capture action)

searchScheme :: GameAppT SideSchemeId
searchScheme = selectJust (SideSchemeIs Sides.mojo39016)

spec :: Spec
spec = describe "Mojo Mania opening scenarios" do
  describe "MaGog" do
    it "starts with both booing crowds, one genre, and the proper scheme" do
      tg <- opening "39001a" ["spider-man"]
      inGame tg do
        selectAny (EnvironmentIs Environments.mojo39003a) `shouldReturn` True
        selectAny (EnvironmentIs Environments.mojo39004a) `shouldReturn` True
        championRatings `shouldReturn` 0
        challengerRatings `shouldReturn` 0
        mainSchemeThreatNow `shouldReturn` 1
        toCardCode . getCardDef <$> getVillain `shouldReturn` "39001a"

    it "resets MaGog at lethal damage, retaining his card and earning ratings" do
      tg <- opening "39001a" ["spider-man"]
      inGame tg do
        vid <- villainIdNow
        let you = player1 tg
        before <- length <$> dealtTo you
        resolve [VillainMessage vid $ VillainDamaged (toSource you) $ toDamage 10 FromAbility]
        getVillainHitPoints vid `shouldReturn` 10
        challengerRatings `shouldReturn` 3
        length <$> dealtTo you `shouldReturn` before + 1
        toCardCode . getCardDef <$> getVillain `shouldReturn` "39001a"

    it "does not announce an enemy defeat when MaGog resets" do
      tg <- opening "39001a" ["spider-man"]
      inGame tg do
        vid <- villainIdNow
        liftIO $ writeIORef tg.ran []
        resolve [VillainMessage vid $ VillainDamaged (toSource $ player1 tg) $ toDamage 10 FromAbility]
        messages <- liftIO $ readIORef tg.ran
        messages `shouldSatisfy` all (not . Text.isInfixOf "DefeatedVillain")
        challengerRatings `shouldReturn` 3

    it "resets a completed scheme and awards the Champion two ratings per player" do
      tg <- opening "39001a" ["spider-man", "captain-marvel"]
      inGame tg do
        placeMainThreat 20
        mainSchemeThreatNow `shouldReturn` 0
        championRatings `shouldReturn` 4

    it "flips the Champion at five ratings per player and draws each player a card" do
      tg <- opening "39001a" ["spider-man"]
      inGame tg do
        before <- length <$> handOf (player1 tg)
        lifted $ rateChampion 5
        selectAny (EnvironmentIs Environments.mojo39003b) `shouldReturn` True
        championRatings `shouldReturn` 5
        length <$> handOf (player1 tg) `shouldReturn` before + 1

    it "loses at ten Champion ratings" do
      tg <- opening "39001a" ["spider-man"]
      inGame tg do
        lifted $ rateChampion 10
        getsGame gameState `shouldReturn` Finished Lost

    it "puts Surprise Contender into play when the Challengers flip" do
      tg <- opening "39001a" ["spider-man"]
      inGame tg do
        lifted $ rateChallengers 5
        selectAny (EnvironmentIs Environments.mojo39004b) `shouldReturn` True
        selectAny (MinionIs Minions.mojo39007 <> MinionEngagedWith (IdentityWithId $ player1 tg)) `shouldReturn` True

    it "wins at ten Challenger ratings" do
      tg <- opening "39001a" ["spider-man"]
      inGame tg do
        lifted $ rateChallengers 10
        getsGame gameState `shouldReturn` Finished Won

    it "Jolt of Adrenaline grants stalwart and retaliate, then adds a reset rating" do
      tg <- opening "39001a" ["spider-man"]
      inGame tg do
        card <- revealNow (player1 tg) Attachments.mojo39005
        vid <- villainIdNow
        getModifiers vid >>= (`shouldSatisfy` elem (KeywordModifier Keyword.Stalwart))
        getModifiers vid >>= (`shouldSatisfy` elem (KeywordModifier $ Keyword.Retaliate 1))
        resolve [VillainMessage vid VillainWouldBeDefeated]
        challengerRatings `shouldReturn` 4
        selectAny (AttachmentIs Attachments.mojo39005) `shouldReturn` False
        encounterDiscard >>= (`shouldSatisfy` elem card)

    it "Surge of Aggression raises both stats and is discarded after a reset" do
      tg <- opening "39001a" ["spider-man"]
      inGame tg do
        void $ revealNow (player1 tg) Attachments.mojo39006
        vid <- villainIdNow
        getVillainAttackValue vid `shouldReturn` 3
        getVillainSchemeValue vid `shouldReturn` 2
        resolve [VillainMessage vid VillainWouldBeDefeated]
        challengerRatings `shouldReturn` 4
        selectAny (AttachmentIs Attachments.mojo39006) `shouldReturn` False

    it "Surprise Contender awards two ratings per player when defeated" do
      tg <- opening "39001a" ["spider-man", "captain-marvel"]
      inGame tg do
        mid <- addMinion (player1 tg) Minions.mojo39007
        resolve [MinionMessage mid $ MinionDamaged (toSource $ player1 tg) $ toDamage 14 FromAbility]
        challengerRatings `shouldReturn` 4

    it "Pump Up the Crowd gains threat for a cheering Champion and rewards its defeat" do
      tg <- opening "39001a" ["spider-man"]
      inGame tg do
        lifted $ rateChampion 5
        card <- revealNow (player1 tg) Sides.mojo39008
        let sid = SideSchemeId $ unCardId $ toCardId card
        getsGame (fmap (Side.sideSchemeThreat . toAttrs) . HashMap.lookup sid . gameSideSchemes) `shouldReturn` Just 5
        resolve [SideSchemeMessage sid $ SideSchemeThwarted (toSource $ player1 tg) 5]
        challengerRatings `shouldReturn` 1

    it "Break a Leg stuns and deals two damage when no ratings are offered" do
      tg <- opening "39001a" ["spider-man"]
      inGame tg do
        let you = player1 tg
        before <- damageOf you
        card <- encounterCard Treacheries.mojo39009
        resolve [RevealEncounterCard you card]
        playerIdentityStunned . toAttrs <$> getPlayer you `shouldReturn` True
        damageOf you `shouldReturn` before + 2
        championRatings `shouldReturn` 0

    it "Defend the Title in alter-ego gives the Champion two ratings" do
      tg <- opening "39001a" ["spider-man"]
      inGame tg do
        card <- encounterCard Treacheries.mojo39010
        resolve [RevealEncounterCard (player1 tg) card]
        championRatings `shouldReturn` 2

    it "Stage Fright confuses and places extra threat when the Challengers lead" do
      tg <- opening "39001a" ["spider-man"]
      inGame tg do
        let you = player1 tg
        lifted $ rateChallengers 1
        card <- encounterCard Treacheries.mojo39011
        resolve [RevealEncounterCard you card]
        playerIdentityConfused . toAttrs <$> getPlayer you `shouldReturn` True
        mainSchemeThreatNow `shouldReturn` 5

  describe "Spiral" do
    it "sets up the permanent search, one Show, and a three-card show deck" do
      tg <- opening "39012a" ["spider-man"]
      inGame tg do
        sid <- searchScheme
        getsGame (fmap (Side.sideSchemeThreat . toAttrs) . HashMap.lookup sid . gameSideSchemes) `shouldReturn` Just 3
        length . pileDeck <$> getPile showDeck `shouldReturn` 3
        codes <- map toCardCode . pileDeck <$> getPile showDeck
        codes `shouldSatisfy` elem "39017"
        deck <- scenarioEncounterDeckNow
        map toCardCode deck `shouldSatisfy` all (`notElem` ["39017", "39016", "39035", "39041", "39047", "39053", "39060", "39066"])

    it "cannot damage escaped Spiral or remove threat from her main scheme" do
      tg <- opening "39012a" ["spider-man"]
      inGame tg do
        let you = player1 tg
        vid <- villainIdNow
        sid <- mainSchemeIdNow
        resolve [VillainMessage vid $ VillainDamaged (toSource you) $ toDamage 5 FromAbility]
        villainDamageNow `shouldReturn` 0
        resolve [MainSchemeMessage sid $ MainSchemeThreatRemoved (toSource you) 1]
        mainSchemeThreatNow `shouldReturn` 1

    it "escaped Spiral schemes instead of attacking" do
      tg <- opening "39012a" ["spider-man"]
      inGame tg do
        vid <- villainIdNow
        before <- damageOf (player1 tg)
        resolve [VillainMessage vid $ VillainAttacks (player1 tg)]
        damageOf (player1 tg) `shouldReturn` before
        mainSchemeThreatNow >>= (`shouldSatisfy` (> 1))

    it "Cornered flips Spiral, reveals a new Show, and returns to the show deck" do
      tg <- opening "39012a" ["spider-man"]
      inGame tg do
        let you = player1 tg
        pile <- getPile showDeck
        let cornered = fromJust $ headMay $ filter ((== "39017") . toCardCode) (pileDeck pile)
        resolve [RevealEncounterCardFrom W.RevealedFromSetAside you cornered]
        toCardCode . getCardDef <$> getVillain `shouldReturn` "39012b"
        codes <- map toCardCode . pileDeck <$> getPile showDeck
        codes `shouldSatisfy` elem "39017"
        encounterDiscard >>= (`shouldSatisfy` all ((/= "39017") . toCardCode))

    it "cornered Spiral escapes after the stage's per-player teleport threshold" do
      tg <- opening "39012a" ["spider-man"]
      inGame tg do
        vid <- villainIdNow
        lifted $ Lifted.flipVillain vid
        resolve [PlaceCounters (VillainRef vid) 2]
        toCardCode . getCardDef <$> getVillain `shouldReturn` "39012b"
        resolve [PlaceCounters (VillainRef vid) 1]
        toCardCode . getCardDef <$> getVillain `shouldReturn` "39012a"
        getCountersOn vid `shouldReturn` 0

    it "preserves Spiral's side when advancing to the next stage" do
      tg <- opening "39012a" ["spider-man"]
      inGame tg do
        vid <- villainIdNow
        lifted $ Lifted.flipVillain vid
        resolve [VillainMessage vid VillainDefeated]
        toCardCode . getCardDef <$> getVillain `shouldReturn` "39013b"
        getCountersOn vid `shouldReturn` 0

    it "removing the last search threat reveals a Show and restores its threat" do
      tg <- opening "39012a" ["spider-man"]
      inGame tg do
        sid <- searchScheme
        pile <- getPile showDeck
        let shows = filter ((/= "39017") . toCardCode) (pileDeck pile)
        lifted $ Lifted.fromDeck showDeck $ Lifted.putOnTopOfEncounterDeck $ take 1 shows
        resolve [SideSchemeMessage sid $ SideSchemeThwarted (toSource $ player1 tg) 3]
        getsGame (fmap (Side.sideSchemeThreat . toAttrs) . HashMap.lookup sid . gameSideSchemes) `shouldReturn` Just 3
        selectAny (SideSchemeIs Sides.mojo39016) `shouldReturn` True

    it "offers the search's damage-paid Hero Action and keeps its permanent scheme" do
      tg <- opening "39012a" ["spider-man"]
      inGame tg do
        let you = player1 tg
        changeForm you
        pass
        sid <- searchScheme
        before <- damageOf you
        choose "search for Spiral" (isAbilityOf sid)
        chooseIf (isTargetChoice sid)
        pass
        resolve []
        damageOf you `shouldReturn` before + 2
        selectAny (SideSchemeIs Sides.mojo39016) `shouldReturn` True
        getsGame (fmap (Side.sideSchemeThreat . toAttrs) . HashMap.lookup sid . gameSideSchemes) `shouldReturn` Just 3

    it "Spiral's Swords starts with three counters and grants that much ATK" do
      tg <- opening "39012a" ["spider-man"]
      inGame tg do
        card <- revealNow (player1 tg) Attachments.mojo39018
        let aid = AttachmentId $ unCardId $ toCardId card
        getCountersOn aid `shouldReturn` 3
        vid <- villainIdNow
        lifted $ Lifted.flipVillain vid
        getVillainAttackValue vid `shouldReturn` 4

    it "Erratic Teleportation adds a teleport counter while cornered" do
      tg <- opening "39012a" ["spider-man"]
      inGame tg do
        vid <- villainIdNow
        lifted $ Lifted.flipVillain vid
        card <- encounterCard Treacheries.mojo39019
        resolve [RevealEncounterCard (player1 tg) card]
        getCountersOn vid `shouldReturn` 1

    it "The Show Must Go On deals each player one card from the current Show set" do
      tg <- opening "39012a" ["spider-man", "captain-marvel"]
      inGame tg do
        before <- traverse (fmap length . dealtTo) tg.seats
        card <- encounterCard Treacheries.mojo39020
        resolve [RevealEncounterCard (player1 tg) card]
        after <- traverse (fmap length . dealtTo) tg.seats
        after `shouldBe` map (+ 1) before

    it "Well-Armed finds and attaches Spiral's Swords when none is in play" do
      tg <- opening "39012a" ["spider-man"]
      inGame tg do
        card <- encounterCard Treacheries.mojo39021
        resolve [RevealEncounterCard (player1 tg) card]
        selectAny (AttachmentIs Attachments.mojo39018) `shouldReturn` True
