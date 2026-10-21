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
import System.Random (mkStdGen)
import Test.Hspec as X

-- | A game dealt from the decks given against the scenario set up, played up to its first
-- question, with the log entries it writes kept.
dealtGame :: ScenarioSetup -> [DeckList] -> Int -> IO (GameApp, IORef [Text])
dealtGame setup decks seed = do
  scenario <- maybe (error "no such scenario") pure $ lookupScenario setup
  players <- either (error . show) pure . sequence =<< traverse loadPlayer decks
  seated <- maybe (error "no decks") pure $ nonEmpty' [setName ("player " <> tshow n) p | (n, p) <- zip [1 :: Int ..] players]
  logged <- newIORef []
  app <- newGameApp (newTable scenario seated) [StartGame] (mkStdGen seed) (\m -> modifyIORef' logged (tshow m :))
  runGameApp app runGameMessages
  pure (app, logged)
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
