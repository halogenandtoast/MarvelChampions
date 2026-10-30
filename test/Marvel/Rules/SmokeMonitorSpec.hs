module Marvel.Rules.SmokeMonitorSpec (spec) where

import Prelude

import Control.Concurrent (threadDelay)
import Control.Exception (displayException)
import Data.List (isInfixOf)
import SmokeMonitor (SmokeQuestionTimeout, withQuestionMonitor)
import TestImport qualified as T
import Test.Hspec

isWatchdog :: SmokeQuestionTimeout -> Bool
isWatchdog = isInfixOf "Smoke watchdog: no new question" . displayException

spec :: Spec
spec = describe "Smoke question watchdog" do
  it "fails when a game asks no question before the deadline" do
    withQuestionMonitor 30000 (\_ -> threadDelay 100000) `shouldThrow` isWatchdog

  it "extends the deadline for new question versions" do
    withQuestionMonitor 150000 (\question -> mapM_ (\v -> question v >> threadDelay 20000) [1..10])
      `shouldReturn` ()

  it "does not treat repeatedly observing one question as progress" do
    withQuestionMonitor 30000 (\question -> mapM_ (\_ -> question 1 >> threadDelay 10000) [1..10 :: Int])
      `shouldThrow` isWatchdog

  it "cancels the watchdog when the game has finished" do
    withQuestionMonitor 30000 (\question -> question 1 >> pure (42 :: Int)) `shouldReturn` 42
    threadDelay 60000

  it "cannot be swallowed by the game's debug-rendering exception handler" do
    tg <- T.testGame ["spider-man"]
    case T.appDebugLogger (T.app tg) of
      Nothing -> expectationFailure "missing test debug logger"
      Just (T.DebugLogger tell) ->
        withQuestionMonitor 30000 (\_ -> tell EndlessDebug) `shouldThrow` isWatchdog

data EndlessDebug = EndlessDebug

instance Show EndlessDebug where
  show _ = repeat 'x'
