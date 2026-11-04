module Marvel.Encounter.ShadowlandSpec (spec) where

import Marvel.Choice qualified as Choice
import Marvel.Window qualified as W
import Marvel.Villain.Types qualified as Villain
import Marvel.Keyword (Keyword(Guard,Villainous))
import Marvel.Treachery.Cards qualified as CoreTreacheries
import Marvel.FearNoEvil.Cards qualified as FNE
import Marvel.SideScheme.Types qualified as Side
import Marvel.Trait qualified as Trait
import Marvel.Shadowland.Position qualified as Position
import Marvel.Area (Pile(..),DeckKey(..))
import Marvel.EncounterCard (gatherEncounterSet)
import Marvel.EncounterSet qualified as Set
import Marvel.Message.Lifted qualified as L
import Marvel.Matchers
import Marvel.Minion.Runner qualified as Minion
import Marvel.Modifier
import Marvel.Query
import Marvel.Ref
import Marvel.Scenario.Types (Attrs(scenarioSetAsideCards))
import Marvel.Shadowland.Cards qualified as Cards
import Data.HashMap.Strict qualified as HashMap
import TestImport

shadowlandGame :: CardCode -> Difficulty -> IO TestGame
shadowlandGame code difficulty = do
  let setup = ScenarioSetup code [Set.SlNinjaSwarm] difficulty
  (app, ran, logged) <- dealtGameLogged setup [starter "spider-man"] 1
  seats <- runGameApp app (getsGame gameSeats)
  let tg = TestGame app ran logged seats
  inGame tg toFirstTurn
  pure tg

spec :: Spec
spec = describe "Shadowland encounter cards" do
  it "contains nine physical cards" do
    tg <- testGame ["spider-man"]
    inGame tg $ length <$> gatherEncounterSet Set.SlNinjaSwarm >>= (`shouldBe` 9)

  it "Black Mountain protects only other ninjas engaged with its player" do
    tg <- testGame ["spider-man", "captain-marvel"]
    inGame tg do
      mountain <- addMinion (player1 tg) Cards.street65028
      protected <- addMinion (player1 tg) Cards.street65030
      other <- addMinion (player2 tg) Cards.street65031
      getModifiers (MinionRef protected) >>= (`shouldSatisfy` elem CannotBeAttacked)
      getModifiers (MinionRef mountain) >>= (liftIO . (`shouldNotContain` [CannotBeAttacked]))
      getModifiers (MinionRef other) >>= (liftIO . (`shouldNotContain` [CannotBeAttacked]))

  it "Dancing Shadows adds hit points to ninjas and blocks its own threat removal" do
    tg <- testGame ["spider-man"]
    inGame tg do
      mid <- addMinion (player1 tg) Cards.street65030
      _ <- revealNow (player1 tg) Cards.street65035
      pass
      sid <- selectJust (SideSchemeIs Cards.street65035)
      getModifiers (SideSchemeRef sid) >>= (`shouldSatisfy` elem ThreatCannotBeRemoved)
      a <- getsGame $ toAttrs . (HashMap.! mid) . gameMinions
      Minion.getModifiedHitPoints a `shouldReturn` 3
      resolve [RemoveFromPlay $ MinionRef mid]
      getModifiers (SideSchemeRef sid) >>= (liftIO . (`shouldNotContain` [ThreatCannotBeRemoved]))

  it "Countless Numbers shuffles only discarded Ninja minions into the deck" do
    tg <- testGame ["spider-man"]
    inGame tg do
      _ <- setEncounterDeck [Cards.street65028] [Cards.street65030, Cards.street65035]
      _ <- revealNow (player1 tg) Cards.street65036
      pass
      discard <- getEncounterDiscard
      liftIO $ map toCardCode discard `shouldContain` ["65035"]
      liftIO $ map toCardCode discard `shouldNotContain` ["65030"]

  it "defeated ninjas stop contributing to teamwork before their replacement checks it" do
    tg <- testGame ["spider-man"]
    inGame tg do
      mid <- addMinion (player1 tg) Cards.street65031
      _ <- setEncounterDeck [Cards.street65034, Cards.street65035] []
      before <- mainSchemeThreatNow
      resolve [MinionMessage mid $ MinionDamaged GameRef $ toDamage 2 FromAbility]
      pass
      selectListCount (MinionIs Cards.street65034) `shouldReturn` 1
      mainSchemeThreatNow `shouldReturn` before

  it "Blinding Powder's Trip deals non-attack damage and confuses the identity" do
    tg <- testGame ["spider-man"]
    inGame tg do
      let p = player1 tg
      _ <- revealNow p Cards.street65050
      pass
      choose "Trip" (isLabel "$label.slTrip")
      pass
      damageOf p `shouldReturn` 2
      identityMatches (IdentityWithId p <> ConfusedIdentity) p `shouldReturn` True

  it "Iron Monk prevents the Avoid option even when its discard can be paid" do
    tg <- testGame ["spider-man"]
    inGame tg do
      let p = player1 tg
      _ <- addMinion p Cards.street65047
      _ <- putInHand p "01087"
      _ <- revealNow p Cards.street65056
      pass
      offers (isLabel "$label.slAvoid") `shouldReturn` False
      offers (isLabel "$label.slTrip") `shouldReturn` True

  it "Creeping Corruption flips on five counters and defeats its owner on five more" do
    tg <- testGame ["spider-man"]
    inGame tg do
      card <- encounterCard Cards.street65072a
      resolve [PutEncounterCardIntoPlay (player1 tg) card]
      uid <- selectJust (UpgradeNamed "Creeping Corruption")
      resolve [PlaceCounters (UpgradeRef uid) 4]
      getCountersOn uid `shouldReturn` 4
      resolve [PlaceCounters (UpgradeRef uid) 1]
      getCountersOn uid `shouldReturn` 0
      selectAny (UpgradeWithId uid <> UpgradeWithTrait Trait.Overpowering) `shouldReturn` True
      resolve [PlaceCounters (UpgradeRef uid) 5]
      getsGame gameState `shouldReturn` Finished Lost


  for_ [("65001a",2),("65038a",25),("65060a",2)] $ \(code,threat) ->
    for_ [Normal,Expert] $ \difficulty ->
      it ("sets up " <> show code <> " on " <> show difficulty) do
        tg <- shadowlandGame code difficulty
        inGame tg $ mainSchemeThreatNow `shouldReturn` threat

  it "keeps both positions on the streets during setup" do
    tg <- shadowlandGame "65001a" Normal
    inGame tg do
      selectListCount (EnvironmentWithTrait Trait.VillainPosition) `shouldReturn` 1
      selectListCount (EnvironmentWithTrait Trait.HeroPosition) `shouldReturn` 1
      Position.villainAtop `shouldReturn` False

  it "a failed then clause leaves the hero's position unchanged" do
    tg <- shadowlandGame "65001a" Normal
    inGame tg do
      let p = player1 tg
      eid <- selectJust (EnvironmentWithTrait Trait.HeroPosition)
      resolve [GiveStatus (IdentityRef p) StunnedStatus]
      resolve [RanAbility p (EnvironmentRef eid) 1 [] NoPayment]
      Position.positionCode Trait.HeroPosition `shouldReturn` Just "65004a"

  it "Shifting Corridors has ten passages and an eight-card trap deck" do
    tg <- shadowlandGame "65038a" Normal
    inGame tg do
      vid <- villainIdNow
      getCountersOn vid `shouldReturn` 10
      length . pileDeck <$> getPile (NamedDeckKey "shadowlandTraps") `shouldReturn` 8

  it "damage to Shifting Corridors removes main-scheme threat" do
    tg <- shadowlandGame "65038a" Normal
    inGame tg do
      vid <- villainIdNow
      Choice.choiceMessages (player1 tg) (DamageCharacter (VillainCharacter vid) GameRef $ toDamage 5 FromAbility) >>= resolve
      mainSchemeThreatNow `shouldReturn` 20
      selectAny (VillainWithCardCode "65037") `shouldReturn` True

  it "removing the last labyrinth threat reveals the selected protege" do
    tg <- shadowlandGame "65038a" Normal
    inGame tg do
      mid <- mainSchemeIdNow
      Choice.choiceMessages (player1 tg) (RemoveThreat GameRef 25 $ SchemeWithId $ SchemeMainSchemeId mid) >>= resolve
      mainSchemeThreatNow `shouldReturn` 1
      selectAny (VillainWithCardCode "65037") `shouldReturn` False
      selectListCount AnyVillain `shouldReturn` 1

  it "Dark Invitation resets its threat and corrupts each hero when completed" do
    tg <- shadowlandGame "65060a" Normal
    inGame tg do
      mid <- mainSchemeIdNow
      resolve [MainSchemeMessage mid (MainSchemePlaceThreat 4)]
      mainSchemeThreatNow `shouldReturn` 0
      uid <- selectJust (UpgradeNamed "Creeping Corruption")
      getCountersOn uid `shouldReturn` 2

  it "defeating a Shadows in the Night protege on side A flips it and deals an encounter card" do
    tg <- shadowlandGame "65001a" Normal
    inGame tg do
      let you = player1 tg
      vid <- villainIdNow
      resolve [ReplaceVillain vid "65013a" Advanced]
      resolve [VillainMessage vid $ VillainDamaged (IdentityRef you) $ toDamage 30 FromAbility]
      selectAny (VillainWithCardCode "65013b") `shouldReturn` True
      current <- villainIdNow
      getVillainHitPoints current `shouldReturn` 18
      Position.villainAtop `shouldReturn` True
      length . playerIdentityEncounterCards . toAttrs <$> getPlayer you `shouldReturn` 1

  it "Dead End removes a passage instead of adding extra threat in Shifting Corridors" do
    tg <- shadowlandGame "65038a" Normal
    inGame tg do
      vid <- villainIdNow
      card <- encounterCard Cards.street65042
      resolve [RevealEncounterCard (player1 tg) card]
      getCountersOn vid `shouldReturn` 9
      sid <- selectJust $ SideSchemeIs Cards.street65042
      getSchemeThreatAmount (SchemeSideSchemeId sid) `shouldReturn` 2

  it "Lord Deathstrike mixes two encounter cards into the trap deck while corridors remain" do
    tg <- shadowlandGame "65038a" Normal
    inGame tg do
      void $ setEncounterDeck (replicate 4 FNE.street60188) []
      void $ addMinion (player1 tg) Cards.street65041
      length . pileDeck <$> getPile (NamedDeckKey "shadowlandTraps") `shouldReturn` 10
      length <$> getEncounterDeck `shouldReturn` 2

  it "defeating Krahllak's first stage advances both the villain and the main scheme" do
    tg <- shadowlandGame "65060a" Normal
    inGame tg do
      let you = player1 tg
      vid <- villainIdNow
      resolve [VillainMessage vid $ VillainDamaged (IdentityRef you) $ toDamage 30 FromAbility]
      selectAny (VillainWithCardCode "65058") `shouldReturn` True
      mainSchemeIdNow >>= \mid -> getsGame (fmap toCardCode . (`cardInPlay` MainSchemeRef mid)) `shouldReturn` Just "65061b"
      current <- villainIdNow
      getVillainHitPoints current `shouldReturn` 21
      mainSchemeThreatNow `shouldReturn` 0

  it "the Chamber reveals a trap only after the first thwart by a player each round" do
    tg <- shadowlandGame "65038a" Normal
    inGame tg do
      let you = player1 tg
      mid <- mainSchemeIdNow
      resolve =<< L.runQueueT (L.capture $ L.removeSchemeThreat you (SchemeMainSchemeId mid) 25)
      changeForm you
      clearHand you
      chamber <- mainSchemeIdNow
      resolve =<< L.runQueueT (L.capture $ L.placeThreatOn GameRef chamber 4)
      traps <- traverse encounterCard [FNE.street60085,FNE.street60085,FNE.street60085]
      resolve =<< L.runQueueT (L.capture $ L.createDeck (NamedDeckKey "shadowlandTraps") traps)
      choose "thwart the Chamber" (isHeroThwart you)
      choose "remove Chamber threat" (isTargetChoice chamber)
      choose "resolve the Chamber trap response" (isAbilityOf chamber)
      chooseIf (isLabel "$label.fneTakeIndirectDamage")
      pass
      length . pileDeck <$> getPile (NamedDeckKey "shadowlandTraps") `shouldReturn` 2
      resolve [IdentityMessage you ReadiedIdentity]
      choose "thwart the Chamber again" (isHeroThwart you)
      choose "remove more Chamber threat" (isTargetChoice chamber)
      resolve []
      length . pileDeck <$> getPile (NamedDeckKey "shadowlandTraps") `shouldReturn` 2

  for_ [False,True] $ \accept ->
    it ("Heart of Shadow chooses corruption or threat after the villain threat step, accept=" <> show accept) do
      tg <- shadowlandGame "65060a" Normal
      inGame tg do
        let you = player1 tg
        vid <- villainIdNow
        resolve [VillainMessage vid $ VillainDamaged (IdentityRef you) $ toDamage 30 FromAbility]
        uid <- selectJust $ UpgradeNamed "Creeping Corruption"
        runNow [CheckWindows [W.Window W.After W.VillainThreatStepFinished]]
        current <- mainSchemeIdNow
        choose "resolve the main scheme response" (isAbilityOf current)
        choose "choose corruption or threat" (isLabel $ if accept then "$label.slCorrupt" else "$label.slResist")
        pass
        getCountersOn uid `shouldReturn` (if accept then 1 else 0)
        mainSchemeThreatNow `shouldReturn` (if accept then 0 else 3)

  it "Lord Deathstrike reveals a trap after the corridors have been replaced" do
    tg <- shadowlandGame "65038a" Normal
    inGame tg do
      let you = player1 tg
      vid <- villainIdNow
      resolve [ReplaceVillain vid "01094" Advanced]
      traps <- traverse encounterCard [FNE.street60085,FNE.street60085]
      resolve =<< L.runQueueT (L.capture $ L.createDeck (NamedDeckKey "shadowlandTraps") traps)
      void $ addMinion you Cards.street65041
      length . pileDeck <$> getPile (NamedDeckKey "shadowlandTraps") `shouldReturn` 1

  it "Sudden Shift mixes the trap discard and three encounter cards before revealing one" do
    tg <- shadowlandGame "65038a" Normal
    inGame tg do
      let you = player1 tg
      clearHand you
      traps <- traverse encounterCard [FNE.street60085,FNE.street60085]
      discarded <- encounterCard Cards.street65051
      resolve =<< L.runQueueT (L.capture do
        L.createDeck (NamedDeckKey "shadowlandTraps") traps
        L.discardCard discarded)
      void $ setEncounterDeck (replicate 4 FNE.street60085) []
      shift <- encounterCard Cards.street65045
      resolve [RevealEncounterCard you shift]
      length . pileDeck <$> getPile (NamedDeckKey "shadowlandTraps") `shouldReturn` 5
      length <$> getEncounterDeck `shouldReturn` 1

  it "Fortress Lockdown changes position and reveals a searched minion" do
    tg <- shadowlandGame "65001a" Normal
    inGame tg do
      mid <- mainSchemeIdNow
      void $ setEncounterDeck [FNE.street60092,FNE.street60188,FNE.street60188,FNE.street60188] []
      resolve [MainSchemeMessage mid $ MainSchemePlaceThreat 5]
      current <- mainSchemeIdNow
      getsGame (fmap toCardCode . (`cardInPlay` MainSchemeRef current)) `shouldReturn` Just "65002b"
      Position.villainAtop `shouldReturn` True
      selectAny (MinionIs FNE.street60092) `shouldReturn` True

  it "Darkness Within does not ask a hero to change to their current form" do
    tg <- testGame ["spider-man"]
    inGame tg do
      let p = player1 tg
      resolve [IdentityMessage p $ ChangedToForm A]
      _ <- revealNow p Cards.street65026
      pass
      identityMatches HeroIdentity p `shouldReturn` True

  it "Whisper uses the selected physical villain card as a minion and returns it set aside on defeat" do
    tg <- shadowlandGame "65038a" Normal
    inGame tg do
      original <- getsGame $ fromMaybe (error "missing chosen protege") . headMay
        . filter (\c -> cdCardType (getCardDef c) == VillainType && toCardCode c /= "65037")
        . scenarioSetAsideCards . toAttrs . gameScenario
      whisper <- encounterCard Cards.street65040
      resolve [RevealEncounterCard (player1 tg) whisper]
      let mid = MinionId $ unCardId $ toCardId original
      selectAny (MinionWithId mid) `shouldReturn` True
      minionMatches (MinionWithKeyword Guard <> MinionWithKeyword Villainous) mid `shouldReturn` True
      resolve [MinionMessage mid $ MinionDamaged GameRef $ toDamage 6 FromAbility]
      selectAny (MinionWithId mid) `shouldReturn` False
      setAsideCards <- getsGame $ scenarioSetAsideCards . toAttrs . gameScenario
      length (filter ((== toCardId original) . toCardId) setAsideCards) `shouldBe` 1
      let returned = filter ((== toCardId original) . toCardId) setAsideCards
      map (cdCardType . getCardDef) returned `shouldBe` [VillainType]
      trapDiscard <- pileDiscard <$> getPile (NamedDeckKey "shadowlandTraps")
      liftIO $ map ecCardId trapDiscard `shouldContain` [ecCardId whisper]
      saved <- getGame
      case fromJSON @Game (toJSON saved) of
        Error err -> liftIO $ expectationFailure err
        Success reloaded -> toJSON reloaded `shouldBe` toJSON saved

  for_ [("an attack gaining keywords",\p -> VillainAttacksGaining p [])
       ,("an attack with a bonus",\p -> VillainAttacksWithBonus p 3 [])
       ,("an attack with extra boosts",\p -> VillainAttacksWithAdditionalBoostCards p 2)
       ,("an attack without boosts",VillainAttacksWithoutBoost)
       ,("a scheme with a bonus",\p -> VillainSchemesWithBonus p 3)] $ \(description,declaration) ->
    it ("Shifting Corridors replaces " <> description <> " with one passage and one trap") do
      tg <- shadowlandGame "65038a" Normal
      inGame tg do
        vid <- villainIdNow
        resolve [VillainMessage vid $ declaration (player1 tg)]
        getCountersOn vid `shouldReturn` 9
        getVillain >>= \v -> Villain.villainBoostCards (toAttrs v) `shouldBe` []

  it "Shifting Corridors spends no passage and reveals no trap when it cannot activate" do
    tg <- shadowlandGame "65038a" Normal
    inGame tg do
      vid <- villainIdNow
      resolve =<< L.runQueueT (L.capture $ L.lastingModifiers GameRef (SourceEntity $ VillainRef vid) [CannotActivate] False)
      resolve [VillainMessage vid $ VillainAttacks (player1 tg)]
      getCountersOn vid `shouldReturn` 10
      length . pileDeck <$> getPile (NamedDeckKey "shadowlandTraps") `shouldReturn` 8

  it "a mixed-in Tracksuit revealed from the trap deck does not release tucked minions" do
    tg <- testGame ["spider-man"]
    inGame tg do
      let p = player1 tg
          traps = NamedDeckKey "shadowlandTraps"
      _ <- setEncounterDeck [FNE.street60200,FNE.street60191] []
      mafia <- encounterCard FNE.street60204
      resolve [RevealEncounterCard p mafia]
      sid <- selectJust (SideSchemeIs FNE.street60204)
      bro <- encounterCard FNE.street60203
      resolve [CreateDeck traps [bro],UsingDeck traps $ DrawAndRevealEncounterCard p]
      held <- getsGame $ Side.sideSchemeHeldEncounterCards . toAttrs . (HashMap.! sid) . gameSideSchemes
      map toCardCode held `shouldBe` ["60200"]
      selectAny (MinionWithId $ minionOf bro) `shouldReturn` True
      normalBro <- encounterCard FNE.street60203
      resolve [RevealEncounterCard p normalBro]
      after <- getsGame $ Side.sideSchemeHeldEncounterCards . toAttrs . (HashMap.! sid) . gameSideSchemes
      length after `shouldBe` 0

  it "surge from a trap-deck card deals from the ordinary encounter deck" do
    tg <- testGame ["spider-man"]
    inGame tg do
      let p = player1 tg
          traps = NamedDeckKey "shadowlandTraps"
      _ <- setEncounterDeck [FNE.street60180] []
      assault <- encounterCard CoreTreacheries.assault
      remaining <- encounterCard Cards.street65030
      resolve [CreateDeck traps [assault,remaining],UsingDeck traps $ DrawAndRevealEncounterCard p]
      dealt <- dealtTo p
      map toCardCode dealt `shouldBe` ["60180"]
      map toCardCode . pileDeck <$> getPile traps `shouldReturn` ["65030"]
