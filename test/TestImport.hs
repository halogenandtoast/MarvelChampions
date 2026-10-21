module TestImport (
  module X,
  module TestImport,
) where

import Data.HashMap.Strict qualified as HashMap
import Data.IORef as X
import Marvel.Decklist as X
import Marvel.Difficulty as X
import Marvel.Game as X
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
import System.Random (mkStdGen)
import Test.Hspec as X
import Control.Monad.IO.Class as X (liftIO)
import Marvel.Card.Def (CardDef)
import Marvel.Card.EncounterCard (EncounterCard)
import Marvel.EncounterCard (genEncounterCard)

-- | A game dealt from the decks given against the scenario set up, played up to its first
-- question, with the messages it runs kept, the latest first.
dealtGame :: ScenarioSetup -> [DeckList] -> Int -> IO (GameApp, IORef [Text])
dealtGame setup decks seed = do
  scenario <- maybe (error "no such scenario") pure $ lookupScenario setup
  players <- either (error . show) pure . sequence $ evalRand (traverse loadPlayer decks) (mkStdGen seed)
  seated <- maybe (error "no decks") pure $ nonEmpty' [setName ("player " <> tshow n) p | (n, p) <- zip [1 :: Int ..] players]
  ran <- newIORef []
  app <- newGameApp (newTable scenario seated) [StartGame] (mkStdGen seed) (const $ pure ())
  let app' = app {appDebugLogger = Just $ DebugLogger \m -> modifyIORef' ran (take 60 . (tshow m :))}
  runGameApp app' runGameMessages
  pure (app', ran)
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

-- | Run these messages now, answering whatever they ask with its first choice (the first
-- forced ability, or not responding), until the game is back at the question it was
-- waiting on or is over.
resolve :: [Message] -> GameAppT ()
resolve msgs = do
  waiting <- openQuestion
  withGame_ $ questionL .~ mempty
  pushAll $ msgs <> [Ask ident q | Just (ident, q) <- [waiting]]
  runGameMessages
  settle waiting (200 :: Int)
 where
  settle waiting n = do
    current <- openQuestion
    finished <- getsGame (isFinished . gameState)
    case current of
      Just (ident, _) | n > 0, current /= waiting, not finished -> do
        answerQuestion ident 0
        settle waiting (n - 1)
      _ -> pure ()
  isFinished = \case
    Finished _ -> True
    _ -> False

-- | A fresh copy of an encounter card.
encounterCard :: CardDef -> GameAppT EncounterCard
encounterCard = genEncounterCard

-- | The players in player order.
players :: GameAppT [IdentityId]
players = getsGame gamePlayerOrder

-- | Play through setup, answering with first choices, to the first player's first turn.
toFirstTurn :: GameAppT ()
toFirstTurn = go (200 :: Int)
 where
  go 0 = error "setup did not reach a turn"
  go n =
    openQuestion >>= \case
      Just (_, ChooseOne cs) | EndTurn `elem` cs -> pure ()
      Just (ident, _) -> answerQuestion ident 0 >> go (n - 1)
      Nothing -> error "no question during setup"
