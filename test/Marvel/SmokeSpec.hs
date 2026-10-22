module Marvel.SmokeSpec (spec) where

import Control.Exception (SomeException, displayException, evaluate, try)
import Data.Text (unpack)
import Control.Monad.IO.Class (liftIO)
import System.Random (mkStdGen, randomR)
import Marvel.EncounterSet (modularSets)
import System.Environment (lookupEnv)
import TestImport
import Data.HashMap.Strict qualified as HashMap

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
        -- ending the turn is chosen rarely, so that cards get played
        let (roll, gen') = randomR (0 :: Int, 9) gen
            others = [i | (i, c) <- zip [0 ..] (choicesOf q), c /= EndTurn]
            pool = if roll == 0 || null others || length others == questionSize q then [0 .. questionSize q - 1] else others
            (k, gen'') = randomR (0, length pool - 1) gen'
        answerQuestion ident (pool !! k)
        go gen'' (n - 1)

spec :: Spec
spec = describe "random play" do
  for_ scenarioCodes \villain ->
    for_ (tables villain) \heroes ->
      for_ [Normal, Expert] \difficulty ->
        it (show villain <> " against " <> show heroes <> " on " <> show difficulty) do
          playSetup (fromMaybe (error "no setup") (recommendedSetup villain)) {difficulty} heroes
  -- each modular set, with each villain, for a pair of heroes
  for_ modularSets \modular ->
    for_ scenarioCodes \villain ->
      it (show villain <> " with " <> show modular) do
        playSetup (ScenarioSetup villain [modular] Normal) ["spider-man", "she-hulk"]
 where
  playSetup setup heroes = do
   -- SMOKE_SEEDS=200 plays many more games, for hunting crashes
   seeds <- maybe 8 read <$> lookupEnv "SMOKE_SEEDS"
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
  tables villain = map pure heroes <> zipWith (\a b -> [a, b]) heroes (drop 1 heroes) <> [take 3 heroes | villain `elem` hastened] <> [take 4 heroes]
  -- the scenarios whose villains random play hardly defeats, and whose later stages are
  -- what matters most
  hastened = ["11001"]
