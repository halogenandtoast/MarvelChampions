module Marvel.SmokeSpec (spec) where

import Control.Exception (SomeException, displayException, evaluate, try)
import Data.Text (unpack)
import Control.Monad.IO.Class (liftIO)
import System.Random (mkStdGen, randomR)
import TestImport

-- | Answer whatever is asked at random, from a seed, for as many answers as given, or
-- until the game ends. The game must not fail on the way.
playRandomly :: Int -> Int -> GameAppT Int
playRandomly seed = go (mkStdGen seed)
 where
  go _ 0 = pure 0
  go gen n =
    openQuestion >>= \case
      Nothing -> pure n
      Just (ident, q) -> do
        let (choice, gen') = randomR (0, questionSize q - 1) gen
        answerQuestion ident choice
        go gen' (n - 1)

spec :: Spec
spec = describe "random play" do
  for_ ["01094", "01113"] \villain ->
    for_ tables \heroes ->
      for_ [Normal, Expert] \difficulty ->
        it (show villain <> " against " <> show heroes <> " on " <> show difficulty) do
          let setup = (fromMaybe (error "no setup") (recommendedSetup villain)) {difficulty}
          for_ [1 .. 8 :: Int] \seed -> do
            (app, ran) <- dealtGame setup (map starter heroes) seed
            result <- try do
              _ <- runGameApp app (playRandomly (seed * 31) 2000)
              g <- runGameApp app getGame
              evaluate (length (show g))
            case result of
              Right _ -> pure ()
              Left (err :: SomeException) -> do
                latest <- readIORef ran
                expectationFailure $ unlines $ ("seed " <> show seed <> ": " <> displayException err) : "the last messages run, latest last:" : map unpack (reverse latest)
 where
  heroes = map fst starterDecks
  tables = map pure heroes <> zipWith (\a b -> [a, b]) heroes (drop 1 heroes) <> [take 4 heroes]
