module Marvel.LogSpec (spec) where

import Data.Text qualified as T
import System.Environment (lookupEnv)
import System.Random (mkStdGen, randomR)
import TestImport

spec :: Spec
spec = describe "the game log" do
  it "tells the rounds, the turns and what is played" do
    let setup = fromMaybe (error "no setup") (recommendedSetup "01094")
    (app, _, logged) <- dealtGameLogged setup [starter "spider-man"] 5
    let go gen (n :: Int) = when (n > 0) do
          runGameApp app openQuestion >>= traverse_ \(ident, q) -> do
            let (choice, gen') = randomR (0, questionSize q - 1) gen
            runGameApp app (answerQuestion ident choice)
            go gen' (n - 1)
    go (mkStdGen 3) 60
    text <- T.concat . map entryText <$> readIORef logged
    lookupEnv "LOG_SAMPLE" >>= traverse_ \path -> writeFile path (T.unpack text)
    for_ ["$log.scenarioBegins", "$log.round", "$log.turn", "$log.phase.villain"] \key ->
      text `shouldSatisfy` T.isInfixOf key
