module Marvel.SmokeSpec (spec) where

import Control.Exception (SomeException, displayException, evaluate, try)
import Data.Text (unpack)
import System.Random (mkStdGen, randomR)
import System.Random.Internal (unStdGen)
import System.Random.SplitMix (unseedSMGen)
import Marvel.EncounterSet (modularSets)
import Marvel.EncounterSet qualified as Encounter
import System.Environment (lookupEnv)
import Text.Read (readMaybe)
import GHC.Clock (getMonotonicTimeNSec)
import Data.Set qualified as Set
import Data.Text qualified as Text
import Data.List (nub)
import SmokeSelection (selectSmokeCases)
import TestImport
import SmokeMonitor (withQuestionMonitor)
import Marvel.Campaign
import Marvel.Scenario.CivilWar (civilWarLeaderCodes)
import Marvel.CivilWar.Types qualified as CW
import Data.HashMap.Strict qualified as HashMap
import Data.ByteString.Lazy qualified as BL
import Marvel.Game.Public (publicGameFor)

-- | Answer whatever is asked at random, from a seed, for as many answers as given, or
-- until the game ends. The game must not fail on the way. Hastened, a villain in play is
-- defeated every so many answers, so that a scenario's later stages are played too.
playRandomly :: (Int -> IO ()) -> (IdentityId -> Int -> IO ()) -> Bool -> Int -> Int -> GameAppT Int
playRandomly onQuestion onAnswer hasten seed = go (mkStdGen seed)
 where
  go _ 0 = pure 0
  go gen n = do
    when (hasten && n `mod` 150 == 0) do
      vs <- getsGame (HashMap.keys . gameVillains)
      let (i, _) = randomR (0, max 0 (length vs - 1)) gen
      for_ (take 1 (drop i vs)) \v -> runNow [VillainMessage v VillainDefeated]
    openQuestion >>= \case
      Nothing -> do
        state <- getsGame gameState
        case state of
          Finished _ -> pure n
          _ -> error "Smoke runner stopped without a question while the game is unfinished"
      Just (ident, q) -> do
        version <- getsGame gameQuestionVersion
        liftIO $ onQuestion version
        -- what the client is sent must be made too, whatever is asked
        view <- liftIO . publicGameFor (const True) =<< getGame
        void $ liftIO $ evaluate (BL.length (encode view))
        -- ending the turn is chosen rarely, so that cards get played
        let (roll, gen') = randomR (0 :: Int, 9) gen
            others = [i | (i, c) <- zip [0 ..] (choicesOf q), c /= EndTurn]
            pool = if roll == 0 || null others || length others == questionSize q then [0 .. questionSize q - 1] else others
            (k, gen'') = randomR (0, length pool - 1) gen'
        liftIO $ onAnswer ident (pool !! k)
        answerQuestion ident (pool !! k)
        go gen'' (n - 1)

saveSmokeSnapshot :: FilePath -> GameApp -> IO ()
saveSmokeSnapshot path app = do
  (game, queue, generator) <- currentState app
  BL.writeFile path $ encode $ object ["game" .= game, "queue" .= queue, "generator" .= unseedSMGen (unStdGen generator)]

spec :: Spec
-- Every example owns its game, queue, logs and seeded random generator.
spec = parallel do
  -- SMOKE_FULL=1 plays every table against every scenario and every modular with every
  -- scenario; by default each scenario gets a few tables, the heroes rotating, and each
  -- modular one scenario, so every deck and set is still played
  full <- runIO $ isJust <$> lookupEnv "SMOKE_FULL"
  focusHeroes <- runIO $ csvEnv "SMOKE_HEROES"
  -- Keep unfinished concurrent decks out of both focused cases and the sample.
  skipHeroes <- runIO $ csvEnv "SMOKE_SKIP_HEROES"
  focusModulars <- runIO $ csvEnv "SMOKE_MODULARS"
  sampleCount <- runIO $ lookupEnv "SMOKE_SAMPLE"
  let focused = not full && (notNull focusHeroes || notNull focusModulars || isJust sampleCount)
      matchesModular modular = Text.toCaseFold (Text.pack $ show modular) `elem` map Text.toCaseFold focusModulars
      tables i villain = if full then allTables villain else nub $ someTables i villain <> [pure hero | hero <- focusHeroes]
      modularSetup villain modular
        | villain `elem` civilWarLeaderCodes = do
            base <- recommendedSetup villain
            let modifiers = [Encounter.StandardII,Encounter.StandardIII,Encounter.ExpertII]
                sets = if modular `elem` modifiers then modular : base.modulars else take 4 $ nub $ modular : base.modulars
                setup = base {modulars = sets}
            _ <- lookupScenario setup
            pure setup
        | otherwise = Just $ ScenarioSetup villain [modular] Normal
      modularVillains i modular =
        let allowed = filter (isJust . (`modularSetup` modular)) scenarioCodes
         in if full || matchesModular modular then allowed else take 1 $ drop (i `mod` max 1 (length allowed)) allowed
      scenarioName villain heroes' difficulty = show villain <> " against " <> show heroes' <> " on " <> show difficulty
      modularName villain modular = show villain <> " with " <> show modular
      competitiveName :: CardCode -> CardCode -> Difficulty -> Int -> String
      competitiveName registration resistance mode players = "Civil War competitive " <> show players <> " seats, " <> show registration <> " vs " <> show resistance <> ", " <> show mode
      competitive =
        [(competitiveName registration resistance mode (length heroes'),setup,heroes')
        | registration <- ["56059","56092","57001"],resistance <- ["56137","56168","57040"],mode <- [Normal,Expert]
        ,heroes' <- [["spider-man","thor"],["spider-man","thor","black-panther","hawkeye"]]
        ,Just reg <- [CW.defaultTeamScenario registration],Just res <- [CW.defaultTeamScenario resistance]
        ,let setup = CivilWarScenarioSetup registration [] mode [] $ Just $ CW.CompetitionSetup reg res]
        <> [(competitiveName "56059" "56168" mode (length heroes') <> ", opposing copies",setup,heroes')
           | mode <- [Normal,Expert]
           , heroes' <- [["spider-man","spider-man"],["spider-man","thor","spider-man","thor"]]
           , Just reg <- [CW.defaultTeamScenario "56059"],Just res <- [CW.defaultTeamScenario "56168"]
           , let setup = CivilWarScenarioSetup "56059" [] mode [] $ Just $ CW.CompetitionSetup reg res]
      campaignName villain n mode = show villain <> " as scenario #" <> show (n + 1) <> ", " <> show mode
      scenarios = [(scenarioName villain heroes' difficulty, villain, heroes', difficulty)
                  | (i, villain) <- zip [0..] scenarioCodes, heroes' <- tables i villain, not (any (`elem` skipHeroes) heroes'), difficulty <- [Normal, Expert]]
      modulars = [(modularName villain modular, villain, modular)
                 | (i, modular) <- zip [0..] modularSets, villain <- modularVillains i modular]
      campaigns = [campaignName villain n mode | campaign <- [madTitansShadow, sinisterMotives, campaignScenarios MutantGenesis, campaignScenarios NextEvolution, campaignScenarios AgeOfApocalypseCampaign, campaignScenarios AgentsOfShieldCampaign, campaignScenarios ShadowlandCampaign], (n, villain) <- zip [0 :: Int ..] campaign, mode <- [StandardCampaign, ExpertCampaign]]
      baselineRelevant = any (`elem` focusHeroes) ["spider-man", "she-hulk"]
      catalogue = [(name, any (`elem` focusHeroes) heroes') | (name, _, heroes', _) <- scenarios]
               <> [(name,any (`elem` focusHeroes) heroes') | (name,_,heroes') <- competitive]
               <> [(name, baselineRelevant || matchesModular modular) | (name, _, modular) <- modulars]
               <> [(name, baselineRelevant || (any (`elem` focusHeroes) ["colossus", "shadowcat"] && name `elem` mutantCampaignCases) || (any (`elem` focusHeroes) ["cable", "domino"] && name `elem` nextEvolutionCampaignCases) || (any (`elem` focusHeroes) ["bishop", "magik"] && name `elem` apocalypseCampaignCases) || (any (`elem` focusHeroes) ["maria-hill", "nick-fury"] && name `elem` shieldCampaignCases) || (any (`elem` focusHeroes) ["daredevil", "jessica-jones", "luke-cage", "spider-man"] && name `elem` shadowlandCampaignCases)) | name <- campaigns]
      mutantCampaignCases = [campaignName villain n mode | (n, villain) <- zip [0 :: Int ..] (campaignScenarios MutantGenesis), mode <- [StandardCampaign, ExpertCampaign]]
      nextEvolutionCampaignCases = [campaignName villain n mode | (n, villain) <- zip [0 :: Int ..] (campaignScenarios NextEvolution), mode <- [StandardCampaign, ExpertCampaign]]
      shadowlandCampaignCases = [campaignName villain n mode | (n, villain) <- zip [0 :: Int ..] (campaignScenarios ShadowlandCampaign), mode <- [StandardCampaign, ExpertCampaign]]
      shieldCampaignCases = [campaignName villain n mode | (n, villain) <- zip [0 :: Int ..] (campaignScenarios AgentsOfShieldCampaign), mode <- [StandardCampaign, ExpertCampaign]]
      apocalypseCampaignCases = [campaignName villain n mode | (n, villain) <- zip [0 :: Int ..] (campaignScenarios AgeOfApocalypseCampaign), mode <- [StandardCampaign, ExpertCampaign]]
  selected <- if not focused then pure Nothing else runIO do
    for_ focusHeroes $ \hero -> unless (hero `elem` heroes) $ fail $ "Unknown SMOKE_HEROES starter deck: " <> unpack hero
    for_ focusModulars $ \modular -> unless (any ((== Text.toCaseFold modular) . Text.toCaseFold . Text.pack . show) modularSets) $
      fail $ "Unknown SMOKE_MODULARS set: " <> unpack modular
    for_ skipHeroes $ \hero -> unless (hero `elem` heroes) $ fail $ "Unknown SMOKE_SKIP_HEROES starter deck: " <> unpack hero
    explicitSeed <- lookupEnv "SMOKE_SAMPLE_SEED"
    -- Only test selection uses this clock seed; each game's RNG remains fixed.
    seed <- maybe (fromIntegral . (`mod` 2147483647) <$> getMonotonicTimeNSec) (pure . read) explicitSeed
    let extra = maybe 50 read sampleCount
        chosen = selectSmokeCases seed extra snd catalogue
    putStrLn $ "Smoke selection: " <> show (length $ filter snd chosen) <> " relevant + "
      <> show (length $ filter (not . snd) chosen) <> " sampled; replay with SMOKE_SAMPLE_SEED=" <> show seed
    pure $ Just $ Set.fromList $ map fst chosen
  let selectedCase name = maybe True (Set.member name) selected
  describe "random play" do
    for_ scenarios $ \(name, villain, heroes', difficulty) ->
      when (selectedCase name) $ it name do
        playSetup (fromMaybe (error "no setup") (recommendedSetup villain)) {difficulty} heroes'
    for_ modulars $ \(name, villain, modular) ->
      when (selectedCase name) $ it name do
        for_ (modularSetup villain modular) $ \setup -> playSetup setup ["spider-man", "she-hulk"]
    for_ competitive $ \(name,setup,heroes') ->
      when (selectedCase name) $ it name $ playSetup setup heroes'
    describe "The Hood alternate encounter sets" do
      for_ ["01094", "24001"] $ \villain ->
        for_ [(Normal,[Encounter.StandardII]), (Expert,[Encounter.StandardII,Encounter.ExpertII]), (Expert,[Encounter.ExpertII])] $ \(mode,sets) ->
          it (show villain <> " with " <> show sets <> " on " <> show mode) $
            playSetup (ScenarioSetup villain (sets <> [Encounter.BeastyBoys,Encounter.BrothersGrimm,Encounter.CrossfiresCrew,Encounter.MisterHydeSet,Encounter.RansackedArmory,Encounter.SinisterSyndicate,Encounter.StateOfEmergency]) mode) ["spider-man","she-hulk"]
    describe "Age of Apocalypse with Standard III" do
      for_ (campaignScenarios AgeOfApocalypseCampaign) \villain ->
        for_ [Normal, Expert] \mode ->
          it (show villain <> " with Standard III on " <> show mode) do
            let setup = fromMaybe (error "no setup") (recommendedSetup villain)
            playSetup setup {difficulty = mode, modulars = Encounter.StandardIII : setup.modulars} ["bishop", "magik"]
    -- each scenario of The Mad Titan's Shadow as a campaign game, every box of its log
    -- checked so each campaign card comes in, expert with the hit points recorded (one
    -- player hurt, one defeated last scenario)
    describe "The Mad Titan's Shadow campaign" do
      for_ (zip [0 ..] madTitansShadow) \(n, villain) ->
        for_ [StandardCampaign, ExpertCampaign] \mode -> do
          let name = campaignName villain n mode
          when (selectedCase name) $ it name do
            let heroes' = ["spider-man", "she-hulk"]
                identities = map ((.hero) . starter) heroes'
                marked = foldr (recordInLog . RecordTitanMark) ((newCampaignOf MadTitansShadow mode identities) {completed = n}) [minBound .. maxBound]
                c = foldr recordInLog marked (zipWith RecordHitPoints identities [5, 0])
            playWith (campaignL ?~ c) (fromMaybe (error "no setup") (recommendedSetup villain)) heroes'
    describe "Sinister Motives campaign" do
      for_ (zip [0 ..] sinisterMotives) \(n, villain) ->
        for_ [StandardCampaign, ExpertCampaign] \mode -> do
          let name = campaignName villain n mode
          when (selectedCase name) $ it name do
            let heroes' = ["spider-man", "she-hulk"]
                identities = map ((.hero) . starter) heroes'
                base = (newCampaignOf SinisterMotives mode identities) {completed = n}
                records =
                  [ RecordReputation ([0, 8, 16, 24, 28] !! n)
                  , RecordCommunityService ["27176", "27177"]
                  , RecordOsbornTech "27147"
                  , RecordOsbornTech "27148"
                  , RecordWakingNightmare 5
                  , RecordLastOnesStanding ["27094", "27095"]
                  ]
                  <> zipWith RecordHitPoints identities [5, 0]
                  <> zipWith RecordShieldTech identities ["27182a", "27183a"]
                  <> [RecordAspectAdvantage identity "01069" 2 | identity <- identities]
                  <> [RecordPlanningAhead identity "01091" | identity <- identities]
                c = foldl' (flip recordInLog) base records
            playWith (campaignL ?~ c) (fromMaybe (error "no setup") (recommendedSetup villain)) heroes'
    describe "Mutant Genesis campaign" do
      for_ (zip [0 ..] $ campaignScenarios MutantGenesis) \(n, villain) ->
        for_ [StandardCampaign, ExpertCampaign] \mode -> do
          let name = campaignName villain n mode
          when (selectedCase name) $ it name do
            let heroes' = ["colossus", "shadowcat"]
                identities = map ((.hero) . starter) heroes'
                base = (newCampaignOf MutantGenesis mode identities) {completed = n}
                records = zipWith RecordMutantRole identities [Defender, Brawler]
                  <> map RecordMutantSchemeDefeated (take n ["32171a", "32172a", "32173a", "32174a", "32175a"])
                  <> (if n > 0 then [RecordMutantJubilee True, RecordMutantCaptives ["32089", "32090"], RecordFuturePast ["32167"] ["32166"]] else [])
                  <> zipWith RecordHitPoints identities [5, 0]
                c = foldl' (flip recordInLog) base records
            playWith (campaignL ?~ c) (fromMaybe (error "no setup") (recommendedSetup villain)) heroes'
    describe "NeXt Evolution campaign" do
      for_ (zip [0 ..] $ campaignScenarios NextEvolution) \(n, villain) ->
        for_ [StandardCampaign, ExpertCampaign] \mode -> do
          let name = campaignName villain n mode
          when (selectedCase name) $ it name do
            let heroes' = ["cable", "domino"]
                identities = map ((.hero) . starter) heroes'
                base = (newCampaignOf NextEvolution mode identities) {completed = n}
                previous = take n nextEvolutionSchemes
                records = zipWith (\i (front, _, _) -> RecordNextEvolutionScheme i front) [1..] previous
                  <> [RecordNextEvolutionEnvironment back | (_, back, _) <- previous]
                  <> [RecordNextEvolutionMarauders ["40070a", "40071a", "40072a"] | n > 0]
                  <> [RecordNextEvolutionMorlocks 1 | n > 0]
                  <> [RecordNextEvolutionHopeDamage i 2 | i <- [3,4], i <= n]
                  <> zipWith RecordHitPoints identities [5, 0]
                c = foldl' (flip recordInLog) base records
            playWith (campaignL ?~ c) (fromMaybe (error "no setup") (recommendedSetup villain)) heroes'
    describe "Age of Apocalypse campaign" do
      for_ (zip [0 ..] $ campaignScenarios AgeOfApocalypseCampaign) \(n, villain) ->
        for_ [StandardCampaign, ExpertCampaign] \mode -> do
          let name = campaignName villain n mode
          when (selectedCase name) $ it name do
            let heroes' = ["bishop", "magik"]
                identities = map ((.hero) . starter) heroes'
                base = (newCampaignOf AgeOfApocalypseCampaign mode identities) {completed = n}
                previous = take n apocalypseMissionCards
                succeeded mission = mode == StandardCampaign || mission `elem` ["45166a", "45169a"]
                rewards mission = case mission of
                  "45166a" -> zipWith RecordApocalypseReward identities ["45176", "45176"]
                  "45167a" -> zipWith RecordApocalypseReward identities $
                    if succeeded mission then ["01093", "01093"] else ["45178", "45178"]
                  "45168a" -> [RecordApocalypseReward identity "01091" | succeeded mission, identity <- identities]
                  "45169a" -> zipWith RecordApocalypseReward identities ["45172", "45173"]
                  _ -> []
                records = zipWith RecordApocalypseMission [1..] previous
                  <> [RecordApocalypseMissionSuccess mission | mission <- previous, succeeded mission]
                  <> map RecordApocalypseOverseer (take n apocalypseOverseers)
                  <> concatMap rewards previous
                  <> [RecordHitPoints identity hp | n > 0, (identity, hp) <- zip identities [5, 0]]
                c = foldl' (flip recordInLog) base records
            playWith (campaignL ?~ c) (fromMaybe (error "no setup") (recommendedSetup villain)) heroes'


    describe "Agents of SHIELD campaign" do
      for_ (zip [0 ..] $ campaignScenarios AgentsOfShieldCampaign) \(n, villain) ->
        for_ [StandardCampaign, ExpertCampaign] \mode -> do
          let name = campaignName villain n mode
          when (selectedCase name) $ it name do
            let heroes' = ["maria-hill", "nick-fury"]
                identities = map ((.hero) . starter) heroes'
                base = (newCampaignOf AgentsOfShieldCampaign mode identities) {completed = n}
                remaining = ["50186","50187","50189","50190","50192","50193"]
                log = noShieldLog
                  { hiddenEvidence = ["50185","50188","50191"]
                  , evidenceEnvelope = drop n remaining
                  , earnedEvidence = take n remaining
                  , boardSecrets = [("50181a",2),("50182a",1),("50183a",0)]
                  , betrayedBoard = ["50181a" | n > 2]
                  , widowRemaining = 2
                  , rescuedCaptives = 1
                  , adaptoidEnvironments = ["50109" | n > 2]
                  , survivingThunderbolts = if n > 3 then ["50143","50152"] else []
                  , evidenceVictories = fromIntegral n
                  }
                records = [RecordShieldLog log | n > 0]
                  <> [RecordHitPoints identity hp | n > 0, (identity,hp) <- zip identities [5,0]]
                c = foldl' (flip recordInLog) base records
            playWith (campaignL ?~ c) (fromMaybe (error "no setup") (recommendedSetup villain)) heroes'


    describe "Shadowland campaign" do
      for_ (zip [0 ..] $ campaignScenarios ShadowlandCampaign) \(n, villain) ->
        for_ [StandardCampaign, ExpertCampaign] \mode -> do
          let name = campaignName villain n mode
          when (selectedCase name) $ it name do
            let heroes' = ["spider-man", "she-hulk"]
                identities = map ((.hero) . starter) heroes'
                base = (newCampaignOf ShadowlandCampaign mode identities) {completed = n}
                records = [RecordShadowlandProtege "65012a" | n > 0]
                  <> [RecordShadowlandOverpowered identity | n > 0, identity <- identities]
                  <> [RecordShadowlandOverpowered identity | n > 1, identity <- take 1 identities]
                  <> [RecordShadowlandPreparedCard identity "01093" | n > 0, identity <- identities]
                  <> [RecordHitPoints identity hp | n > 0, (identity,hp) <- zip identities [5,0]]
                c = foldl' (flip recordInLog) base records
            playWith (campaignL ?~ c) (fromMaybe (error "no setup") (recommendedSetup villain)) heroes'

 where
  csvEnv name = maybe [] (filter (not . Text.null) . map Text.strip . Text.splitOn "," . Text.pack) <$> lookupEnv name
  playSetup = playWith id
  playWith f setup heroes = do
   -- SMOKE_SEEDS=200 plays many more games, for hunting crashes
   seeds <- maybe 4 read <$> lookupEnv "SMOKE_SEEDS"
   for_ [1 .. seeds :: Int] \seed -> do
      snapshotPath <- lookupEnv "SMOKE_FAILURE_SNAPSHOT"
      recentAnswer <- newIORef Nothing
      checkpointVersion <- (>>= readMaybe) <$> lookupEnv "SMOKE_CHECKPOINT_VERSION"
      checkpointPath <- lookupEnv "SMOKE_CHECKPOINT_SNAPSHOT"
      recent <- newIORef Nothing
      recentApp <- newIORef Nothing
      result <- try $ withQuestionMonitor 5000000 $ \onQuestion -> do
        (app, ran, _) <- dealtGameWith f (withTestModulars setup) (map starter heroes) seed
        writeIORef recent (Just ran)
        writeIORef recentApp (Just app)
        let recordQuestion version = do
              onQuestion version
              when (checkpointVersion == Just version) $ for_ checkpointPath $ \path -> saveSmokeSnapshot path app
            recordAnswer player choice = for_ snapshotPath $ \_ -> do
              (game, queue, generator) <- currentState app
              writeIORef recentAnswer $ Just $ object ["game" .= game, "queue" .= queue, "generator" .= unseedSMGen (unStdGen generator), "player" .= player, "choice" .= choice]
        _ <- runGameApp app (playRandomly recordQuestion recordAnswer (setup.scenarioCode `elem` hastened) (seed * 31) 2000)
        g <- runGameApp app getGame
        evaluate (length (show g))
      case result of
        Right _ -> pure ()
        Left (err :: SomeException) -> do
          -- Opt in for a single focused example: preserve the exact failing
          -- calculation for replay without dumping a whole game into Hspec.
          for_ snapshotPath \path -> do
            saved <- readIORef recentApp
            for_ saved $ saveSmokeSnapshot path
            previous <- readIORef recentAnswer
            for_ previous $ BL.writeFile (path <> ".previous.json") . encode
          latest <- readIORef recent >>= maybe (pure []) readIORef
          expectationFailure $ unlines $ ("scenario " <> show setup.scenarioCode <> ", heroes " <> show heroes <> ", difficulty " <> show setup.difficulty <> ", seed " <> show seed <> ": " <> displayException err <> "\n" <> show err) : "the last messages run, latest last:" : map unpack (reverse latest)
  heroes = map fst starterDecks
  -- The Once and Future Kang splits the table into an area a player: played with three
  -- too
  allTables villain = map pure heroes <> zipWith (\a b -> [a, b]) heroes (drop 1 heroes) <> [take 3 heroes | villain `elem` hastened] <> [take 4 heroes]
  -- one hero, a pair and four, starting further along the heroes for each scenario
  someTables i villain =
    let from n k = take k (drop (n `mod` length heroes) (cycle heroes))
     in [from (i * 7) 1, from (i * 7 + 1) 2, from (i * 7 + 3) 4] <> [from (i * 7 + 2) 3 | villain `elem` hastened]
  -- the scenarios whose villains random play hardly defeats, and whose later stages are
  -- what matters most
  hastened = ["11001", "21160"]
