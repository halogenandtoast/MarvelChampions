module Marvel.LogSpec (spec) where

import Data.Text qualified as T
import System.Environment (lookupEnv)
import System.Random (mkStdGen, randomR)
import TestImport

spec :: Spec
spec = describe "the game log" do
  it "explains basic actions canceled by statuses without logging a successful action" do
    tg <- testGame ["spider-man"]
    inGame tg do
      let p = player1 tg
      changeForm p
      pass
      runDebug $ CardSpecific (IdentityRef p) "debugSetValues" $ object ["stunned" .= True, "confused" .= True]
      runNow [IdentityMessage p (SideMessage Attacked)]
      runNow [IdentityMessage p (SideMessage Thwarted)]
    told <- T.concat . map entryText <$> readIORef tg.logged
    told `shouldSatisfy` T.isInfixOf "$log.attackCanceledByStun"
    told `shouldSatisfy` T.isInfixOf "$log.thwartCanceledByConfusion"
    told `shouldSatisfy` (not . T.isInfixOf "$log.basicAttack")
    told `shouldSatisfy` (not . T.isInfixOf "$log.basicThwart")

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
