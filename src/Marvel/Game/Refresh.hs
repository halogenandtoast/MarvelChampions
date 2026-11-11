-- Saved turn menus contain concrete abilities from the engine version that asked
-- them. Rebuild only turn menus so loading or undoing cannot revive stale actions.
module Marvel.Game.Refresh (refreshTurnQuestions) where

import Marvel.Prelude
import Data.HashMap.Strict qualified as HashMap
import Marvel.Game
import Marvel.ActiveCost (filterCosts, flattenCosts, resourceCostPaid)
import Marvel.ActiveCost.Types
import Marvel.Card
import Marvel.Cost.Types (Cost (DynamicResourceCost))
import Marvel.Resource.Types (Resource (Energy))
import Marvel.Message.Lifted
import Marvel.Question

refreshTurnQuestions :: Game -> IO Game
refreshTurnQuestions saved = do
  original <- refreshLightningStrikePayment saved
  let turnPlayers = [you | (you, QuestionLabel "$label.yourTurn" _ _) <- HashMap.toList $ gameQuestion original]
  foldlM refresh original turnPlayers
 where
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

-- Lightning Strike used to combine its printed play cost and its effect's energy
-- spend. Preserve the resources already generated while repairing old payments.
refreshLightningStrikePayment :: Game -> IO Game
refreshLightningStrikePayment game = case gameActiveCost game of
  cost : rest
    | ForCard card <- activeCostTarget cost
    , toCardCode card == "06006"
    , DynamicResourceCost (Just Energy) `elem` flattenCosts (activeCostCost cost) -> do
        let updated = cost {activeCostCost = filterCosts (/= DynamicResourceCost (Just Energy)) (activeCostCost cost)}
            repaired = game {gameActiveCost = updated : rest}
        app <- newApp repaired []
        covered <- runGameApp app $ resourceCostPaid updated
        pure $ if covered
          then repaired {gameQuestion = HashMap.adjust allowFinish (activeCostIdentityId cost) (gameQuestion repaired)}
          else repaired
  _ -> pure game
 where
  allowFinish = \case
    QuestionLabel "$label.pay" source (ChooseOne choices) ->
      QuestionLabel "$label.pay" source $ ChooseOne $ if FinishPayment `elem` choices then choices else choices <> [FinishPayment]
    question -> question
