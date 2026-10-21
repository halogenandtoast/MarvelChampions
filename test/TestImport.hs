module TestImport (
  module X,
  module TestImport,
) where

import Data.HashMap.Strict qualified as HashMap
import Data.IORef as X
import Marvel.Decklist as X
import Marvel.Difficulty as X
import Marvel.Game as X
import Marvel.Game.Run as X
import Marvel.Id as X
import Marvel.Identity as X (setName)
import Marvel.Message as X
import Marvel.Prelude as X
import Marvel.Question as X
import Marvel.Queue as X
import Marvel.Scenario as X
import Marvel.Scenario.Types as X (ScenarioSetup (..))
import Control.Monad.Random (evalRand)
import Marvel.Debug as X
import Marvel.Frontend as X
import Marvel.Log as X (LogEntry, entryText)
import System.Random (mkStdGen)
import Test.Hspec as X

-- | A game dealt from the decks given against the scenario set up, played up to its first
-- question, with the messages it runs kept, the latest first.
dealtGame :: ScenarioSetup -> [DeckList] -> Int -> IO (GameApp, IORef [Text])
dealtGame setup decks seed = do
  (app, ran, _) <- dealtGameLogged setup decks seed
  pure (app, ran)

-- | The same, keeping the log entries it writes too, oldest first.
dealtGameLogged :: ScenarioSetup -> [DeckList] -> Int -> IO (GameApp, IORef [Text], IORef [LogEntry])
dealtGameLogged setup decks seed = do
  scenario <- maybe (error "no such scenario") pure $ lookupScenario setup
  players <- either (error . show) pure . sequence $ evalRand (traverse loadPlayer decks) (mkStdGen seed)
  seated <- maybe (error "no decks") pure $ nonEmpty' [setName ("player " <> tshow n) p | (n, p) <- zip [1 :: Int ..] players]
  ran <- newIORef []
  logged <- newIORef []
  app <- newGameApp (newTable scenario seated) [StartGame] (mkStdGen seed) \case
    Logged e -> modifyIORef' logged (<> [e])
    _ -> pure ()
  let app' = app {appDebugLogger = Just $ DebugLogger \m -> modifyIORef' ran (take 60 . (tshow m :))}
  runGameApp app' runGame
  pure (app', ran, logged)
 where
  nonEmpty' = \case
    [] -> Nothing
    (x : xs) -> Just (x :| xs)

starter :: Text -> DeckList
starter hero = fromMaybe (error $ "no starter deck for " <> show hero) $ lookupStarterDeck hero

-- | Who is asked what, if anyone is.
openQuestion :: GameAppT (Maybe (IdentityId, Question))
openQuestion = getsGame (headMay' . HashMap.toList . gameQuestion)
 where
  headMay' = \case
    [] -> Nothing
    (x : _) -> Just x

questionSize :: Question -> Int
questionSize = \case
  ChooseOne cs -> length cs
  ChooseN _ cs -> length cs
  ChooseOneAtATime cs -> length cs
  ChoosePlayerOrder (Unsorted xs) _ -> length xs
  QuestionLabel _ _ q -> questionSize q
