module Marvel.CivilWar.ScenarioSpec (spec) where

import Data.HashMap.Strict qualified as HashMap
import Data.List (nub)
import Marvel.Area (AreaId (..),DeckKey (..),Pile (..))
import Marvel.CivilWar.Types qualified as CW
import Marvel.CivilWar.Cards qualified as C
import Marvel.EncounterSet qualified as E
import Marvel.EncounterCard (gatherEncounterSet)
import Marvel.Identity (identityObligations)
import Marvel.MainScheme.Types qualified as Main
import Marvel.Matchers
import Marvel.Treachery.Cards qualified as T
import Marvel.Query
import Marvel.Scenario.CivilWar
import Marvel.Scenario.Types qualified as S
import Test.Hspec qualified as H
import TestImport

nativeGame :: ScenarioSetup -> [Text] -> IO TestGame
nativeGame setup heroes = do
  (app,ran,logged) <- dealtGameLogged setup (map starter heroes) 1
  seats <- runGameApp app (getsGame gameSeats)
  let tg = TestGame app ran logged seats
  inGame tg toFirstTurn
  pure tg

competitionSetup :: CardCode -> CardCode -> Difficulty -> ScenarioSetup
competitionSetup registration resistance difficulty = CivilWarScenarioSetup registration [] difficulty [] $
  Just $ CW.CompetitionSetup (defaults registration) (defaults resistance)
 where defaults code = fromMaybe (error "missing default scenario") $ CW.defaultTeamScenario code

spec :: Spec
spec = describe "Civil War native scenario construction" do
  for_ (filter (`elem` E.modularSets) $ CW.registrationModulars <> CW.resistanceModulars) $ \modular ->
    it ("gathers the complete five physical cards of " <> show modular) do
      tg <- testGame ["spider-man"]
      inGame tg do
        cards <- gatherEncounterSet modular
        length cards `shouldBe` 5
        length (nub $ map ecCardId cards) `shouldBe` 5
        map (cdEncounterSet . getCardDef) cards `shouldSatisfy` all (== Just modular)

  for_ civilWarLeaderCodes $ \code -> for_ [Normal,Expert] $ \difficulty -> for_ [1,2] $ \count ->
    it ("deals the cooperative scenario " <> show (code,difficulty,count) <> " with its own setup and encounter cards") do
      tg <- nativeGame (ScenarioSetup code [] difficulty) (take count ["spider-man","thor"])
      inGame tg do
        let defaults = fromMaybe (error "missing defaults") $ CW.defaultTeamScenario code
            first = testHead $ CW.teamScenarioMainSchemes defaults
            second = testHead $ testTail $ CW.teamScenarioMainSchemes defaults
        [vid] <- getVillainsInArea (AreaId 0)
        getsGame (fmap (toCardCode . getCardDef) . HashMap.lookup vid . gameVillains) `shouldReturn` Just (leaderStage code difficulty)
        mains <- getsGame $ HashMap.elems . gameMainSchemes
        map (toCardCode . getCardDef) mains `shouldBe` [first]
        map (Main.mainSchemeThreat . toAttrs) mains `shouldBe` [if count == 1 then 0 else 4]
        getsGame (S.scenarioMainSchemes . toAttrs . gameScenario) `shouldReturn` [second]
        deck <- getEncounterDeck
        dealt <- concatMapM (fmap playerIdentityEncounterCards . playerAttrs) =<< getAllPlayers
        let families = map (cdEncounterSet . getCardDef) (deck <> dealt)
        length dealt `shouldBe` (if code `elem` ["56092","56137"] then count else 0)
        length (filter (== Just E.Standard) families) `shouldBe` 7
        length (filter (== Just E.ExpertSet) families) `shouldBe` (if difficulty == Expert then 3 else 0)
        length (filter isNothing families) `shouldBe` count
        for_ (CW.teamScenarioModulars defaults) $ \modular -> (modular,length (filter (== Just modular) families)) `shouldBe` (modular,5)
        attachments <- selectList $ AttachmentOnEnemy $ EnemyVillainId vid
        length attachments `shouldBe` 1
        sides <- selectList AnySideScheme
        length sides `shouldBe` 1
        rewards <- getSetAsidePlayerCards
        filter ((`elem` concatMap CW.leaderBasicCards civilWarLeaderCodes) . toCardCode) rewards `shouldBe` []

  for_ ["56059","56092"] $ \registration -> for_ ["56137","56168"] $ \resistance -> for_ [Normal,Expert] $ \difficulty ->
    it ("exchanges independently built scenarios and separates their physical cards: " <> show (registration,resistance,difficulty)) do
      tg <- nativeGame (competitionSetup registration resistance difficulty) ["spider-man","thor"]
      inGame tg do
        live <- getCompetition
        c <- maybe (error "missing native competition") pure live
        for_ [(CW.Registration,resistance),(CW.Resistance,registration)] $ \(team,code) -> do
          let actor = CW.teamFirstPlayer c team
              defaults = fromMaybe (error "missing defaults") $ CW.defaultTeamScenario code
          Just vid <- getActiveVillainIn (CW.teamArea team)
          getsGame (fmap (toCardCode . getCardDef) . HashMap.lookup vid . gameVillains) `shouldReturn` Just (leaderStage code difficulty)
          key <- getDeckKeyFor actor
          key `shouldBe` VillainDeckKey vid
          pile <- getPile key
          dealt <- concatMapM (fmap playerIdentityEncounterCards . playerAttrs) $ CW.teamPlayers c team
          let families = map (cdEncounterSet . getCardDef) (pileDeck pile <> dealt)
          length dealt `shouldBe` (if code `elem` ["56092","56137"] then 1 else 0)
          length (filter (== Just E.CwStandardPvp) families) `shouldBe` 4
          length (filter (== Just E.Standard) families) `shouldBe` 0
          for_ (CW.teamScenarioModulars defaults) $ \modular -> (modular,length (filter (== Just modular) families)) `shouldBe` (modular,5)
          length (filter (== Just E.ExpertSet) families) `shouldBe` (if difficulty == Expert then 3 else 0)
          identity <- getIdentity actor
          let obligationCodes = map toCardCode $ identityObligations identity
          length [card | card <- pileDeck pile,toCardCode card `elem` obligationCodes] `shouldBe` length obligationCodes
          for_ (pileDeck pile <> dealt) $ \card -> getsGame (HashMap.lookup (ecCardId card) . S.scenarioCardDecks . toAttrs . gameScenario) `shouldReturn` Just key
          [sid] <- getMainSchemesIn (CW.teamArea team)
          getsGame (fmap (toCardCode . getCardDef) . HashMap.lookup sid . gameMainSchemes) `shouldReturn` headMay (CW.teamScenarioMainSchemes defaults)
          aside <- getSetAsideEncounterCardsForDeck key
          map (cdEncounterSet . getCardDef) aside `shouldSatisfy` all (== Just (E.getNemesisSet $ toCardCode identity))
          length aside `shouldBe` 5
          sides <- asYou actor $ selectList AnySideScheme
          length sides `shouldBe` 1
        getsGame (S.scenarioEncounterDeck . toAttrs . gameScenario) `shouldReturn` []
        rewards <- getSetAsidePlayerCards
        length rewards `shouldBe` 8
        map pcOwner rewards `shouldBe` replicate 8 Nothing
        length (nub $ map pcCardId rewards) `shouldBe` 8
        keys <- getDeckKeys
        keys `shouldSatisfy` all (`notElem` map CW.pendingEncounterDeck [CW.Registration,CW.Resistance])

  it "keeps opposing copies of the same hero's nemesis set physically separate" do
    tg <- nativeGame (competitionSetup "56059" "56168" Normal) ["spider-man","spider-man"]
    inGame tg do
      let one = player1 tg
          two = seats tg !! 1
      first <- selectList (NemesisSetFor one)
      second <- selectList (NemesisSetFor two)
      length first `shouldBe` 5
      length second `shouldBe` 5
      map ecCardId first `shouldSatisfy` all (`notElem` map ecCardId second)
      length <$> getNemesisMinionsOutOfPlay one `shouldReturn` 1
      length <$> getNemesisMinionsOutOfPlay two `shouldReturn` 1
      shadow <- encounterCard T.shadowOfThePast
      resolve [RevealEncounterCard one shadow]
      selectList (NemesisSetFor one) `shouldReturn` []
      selectList (NemesisSetFor two) `shouldReturn` second
      otherShadow <- encounterCard T.shadowOfThePast
      resolve [RevealEncounterCard two otherShadow]
      selectList (NemesisSetFor two) `shouldReturn` []
      for_ [one,two] $ \actor -> do
        minions <- asYou actor $ selectList AnyMinion
        length minions `shouldBe` 1

  it "sets up four seats as two teams with independent per-player values" do
    tg <- nativeGame (competitionSetup "56092" "56168" Normal) ["spider-man","thor","black-panther","hawkeye"]
    inGame tg do
      Just competition <- getCompetition
      for_ [CW.Registration,CW.Resistance] $ \team -> do
        let first = CW.teamFirstPlayer competition team
            code = CW.teamScenarioLeader $ CW.scenarioForTeam (CW.competitiveSetup competition) (CW.otherTeam team)
            (_,_,hp,_) = C.printedLeaderStats code
        asYou first getStartingPlayerCount `shouldReturn` 2
        Just leader <- getActiveVillainIn (CW.teamArea team)
        getEnemyRemainingHitPoints (EnemyVillainId leader) `shouldReturn` (2 * fromIntegral hp)
        [main] <- getMainSchemesIn (CW.teamArea team)
        getsGame (fmap (Main.mainSchemeThreat . toAttrs) . HashMap.lookup main . gameMainSchemes) `shouldReturn` Just 4
        [side] <- asYou first $ selectList AnySideScheme
        sideSchemeThreat side `shouldReturn` 8
        pile <- getPile =<< getDeckKeyFor first
        length [card | card <- pileDeck pile,isNothing $ cdEncounterSet $ getCardDef card] `shouldBe` 2

  it "preserves the native scenario and remaining setup through a Game JSON round trip" do
    let setup = competitionSetup "56059" "56137" Expert
    (app,_) <- dealtGameBeforeSetup id setup [starter "spider-man",starter "thor"] 1 (const $ pure ())
    runGameApp app do
      saved <- getsGame toJSON
      restored <- case fromJSON saved of
        Success game -> pure game
        Error err -> error err
      (toJSON (restored :: Game) == saved) `shouldBe` True
      withGame_ $ const restored
      toFirstTurn
      for_ [CW.Registration,CW.Resistance] $ \team -> do
        getActiveVillainIn (CW.teamArea team) >>= (`shouldSatisfy` isJust)
        length <$> getMainSchemesIn (CW.teamArea team) `shouldReturn` 1
      getSetAsidePlayerCards >>= \cards -> length cards `shouldBe` 8

  for_ [False,True] $ \opponent ->
    it ("puts the Pool encounter cards only in their user's team scenario: opponent " <> show opponent) do
      let heroes = if opponent then ["spider-man","deadpool"] else ["deadpool","thor"]
          poolTeam = if opponent then CW.Resistance else CW.Registration
      tg <- nativeGame (competitionSetup "56059" "56137" Normal) heroes
      inGame tg do
        Just competition <- getCompetition
        for_ [CW.Registration,CW.Resistance] $ \team -> do
          key <- getDeckKeyFor $ CW.teamFirstPlayer competition team
          pile <- getPile key
          length [card | card <- pileDeck pile,toCardCode card == "44037"] `shouldBe` (if team == poolTeam then 1 else 0)
          aside <- getSetAsideEncounterCardsForDeck key
          length [card | card <- aside,toCardCode card `elem` ["44038","44039","44040","44041","44042"]] `shouldBe` (if team == poolTeam then 6 else 0)

  it "earns its own leader's physical rewards through the native Choosing Sides flow" do
    tg <- nativeGame (competitionSetup "56059" "56137" Normal) ["spider-man","thor"]
    inGame tg do
      let you = player1 tg
      [sid] <- selectList AnySideScheme
      resolve [SideSchemeMessage sid $ SideSchemeThreatRemoved (IdentityRef you) 4]
      let oid = ObligationId $ unSideSchemeId sid
      choose "use Now It's Personal" (isAbilityOf $ ObligationRef oid)
      choose "take High-Tech Suit from the registration pool" (\case CardLabel code _ -> code == "56131"; _ -> False)
      choose "take The Futurist from the same pool" (\case CardLabel code _ -> code == "56129"; _ -> False)
      pass
      cards <- filter ((`elem` ["56131","56129"]) . toCardCode) <$> handOf you
      length cards `shouldBe` 2
      map pcOwner cards `shouldBe` [Just you,Just you]
      aside <- getSetAsidePlayerCards
      length aside `shouldBe` 6
      filter ((`elem` ["56131","56129"]) . toCardCode) aside `shouldBe` []

  it "advances a cooperative custom main scheme without changing the chosen leader" do
    let setup = CivilWarScenarioSetup "56059" [] Normal ["56121b","56124b"] Nothing
    tg <- nativeGame setup ["spider-man"]
    inGame tg do
      Just leader <- getActiveVillainIn (AreaId 0)
      [main] <- getMainSchemesIn (AreaId 0)
      resolve [AdvanceMainScheme main]
      [next] <- getMainSchemesIn (AreaId 0)
      getsGame (fmap (toCardCode . getCardDef) . HashMap.lookup next . gameMainSchemes) `shouldReturn` Just "56124b"
      getActiveVillainIn (AreaId 0) `shouldReturn` Just leader

  it "rejects an opposing-faction cooperative modular set" do
    let setup = ScenarioSetup "56059" [E.CwNewAvengers,E.CwTheInitiative,E.CwMariaHillModular] Normal
    validateCivilWarSetup setup `H.shouldBe` Left "error.cwModularFaction"
    isNothing (lookupScenario setup) `H.shouldBe` True

  it "accepts three cooperative modules including a conventional modular set" do
    let setup = ScenarioSetup "56059" [E.BombScare,E.CwTheInitiative,E.CwMariaHillModular] Normal
    validateCivilWarSetup setup `H.shouldBe` Right ()
    isJust (lookupScenario setup) `H.shouldBe` True
