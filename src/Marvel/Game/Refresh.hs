-- Saved turn menus contain concrete abilities from the engine version that asked
-- them. Rebuild only turn menus so loading or undoing cannot revive stale actions.
module Marvel.Game.Refresh (refreshTurnQuestions) where

import Marvel.Prelude
import Data.HashMap.Strict qualified as HashMap
import Marvel.Game
import Marvel.Message.Lifted
import Marvel.Question

refreshTurnQuestions :: Game -> IO Game
refreshTurnQuestions original = foldlM refresh original turnPlayers
 where
  turnPlayers = [you | (you, QuestionLabel "$label.yourTurn" _ _) <- HashMap.toList $ gameQuestion original]
  refresh game you = do
    app <- newApp game []
    rebuilt <- runGameApp app do
      withGame_ $ questionL .~ mempty
      runQueueT $ identityStep you PlayerTurnOption
      runGameMessages
      getGame
    pure $ case HashMap.lookup you (gameQuestion rebuilt) of
      Just question -> game {gameQuestion = HashMap.insert you question (gameQuestion game)}
      Nothing -> game
