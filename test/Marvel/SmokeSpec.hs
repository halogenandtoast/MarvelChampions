module Marvel.SmokeSpec (spec) where

import Control.Exception (SomeException, displayException, evaluate, try)
import Data.Text (unpack)
import Control.Monad.IO.Class (liftIO)
import System.Random (mkStdGen, randomR)
import Marvel.EncounterSet (modularSets)
import System.Environment (lookupEnv)
import TestImport
import Data.HashMap.Strict qualified as HashMap
import Data.Aeson (encode)
import Data.ByteString.Lazy qualified as BL
import Marvel.Game.Public (publicGameFor)

-- | Answer whatever is asked at random, from a seed, for as many answers as given, or
-- until the game ends. The game must not fail on the way. Hastened, a villain in play is
-- defeated every so many answers, so that a scenario's later stages are played too.
playRandomly :: Bool -> Int -> Int -> GameAppT Int
playRandomly hasten seed = go (mkStdGen seed)
 where
  go _ 0 = pure 0
  go gen n = do
    when (hasten && n `mod` 150 == 0) do
      vs <- getsGame (HashMap.keys . gameVillains)
      let (i, _) = randomR (0, max 0 (length vs - 1)) gen
      for_ (take 1 (drop i vs)) \v -> runNow [VillainMessage v VillainDefeated]
    openQuestion >>= \case
      Nothing -> pure n
      Just (ident, q) -> do
        -- what the client is sent must be made too, whatever is asked
        view <- liftIO . publicGameFor (const True) =<< getGame
        void $ liftIO $ evaluate (BL.length (encode view))
        -- ending the turn is chosen rarely, so that cards get played
        let (roll, gen') = randomR (0 :: Int, 9) gen
            others = [i | (i, c) <- zip [0 ..] (choicesOf q), c /= EndTurn]
            pool = if roll == 0 || null others || length others == questionSize q then [0 .. questionSize q - 1] else others
            (k, gen'') = randomR (0, length pool - 1) gen'
        answerQuestion ident (pool !! k)
        go gen'' (n - 1)

spec :: Spec
spec = do
  -- SMOKE_FULL=1 plays every table against every scenario and every modular with every
  -- scenario; by default each scenario gets a few tables, the heroes rotating, and each
  -- modular one scenario, so every deck and set is still played
  full <- runIO $ isJust <$> lookupEnv "SMOKE_FULL"
  describe "random play" do
    for_ (zip [0 ..] scenarioCodes) \(i, villain) ->
      for_ (if full then allTables villain else someTables i villain) \heroes ->
        for_ [Normal, Expert] \difficulty ->
          it (show villain <> " against " <> show heroes <> " on " <> show difficulty) do
            playSetup (fromMaybe (error "no setup") (recommendedSetup villain)) {difficulty} heroes
    for_ (zip [0 ..] modularSets) \(i, modular) ->
      for_ (if full then scenarioCodes else [scenarioCodes !! (i `mod` length scenarioCodes)]) \villain ->
        it (show villain <> " with " <> show modular) do
          playSetup (ScenarioSetup villain [modular] Normal) ["spider-man", "she-hulk"]
 where
  playSetup setup heroes = do
   -- SMOKE_SEEDS=200 plays many more games, for hunting crashes
   seeds <- maybe 4 read <$> lookupEnv "SMOKE_SEEDS"
   for_ [1 .. seeds :: Int] \seed -> do
      (app, ran) <- dealtGame setup (map starter heroes) seed
      result <- try do
        _ <- runGameApp app (playRandomly (setup.scenarioCode `elem` hastened) (seed * 31) 2000)
        g <- runGameApp app getGame
        evaluate (length (show g))
      case result of
        Right _ -> pure ()
        Left (err :: SomeException) -> do
          latest <- readIORef ran
          expectationFailure $ unlines $ ("seed " <> show seed <> ": " <> displayException err <> "\n" <> show err) : "the last messages run, latest last:" : map unpack (reverse latest)
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
