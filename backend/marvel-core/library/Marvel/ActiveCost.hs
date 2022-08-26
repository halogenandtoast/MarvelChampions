module Marvel.ActiveCost where

import Marvel.Prelude

import Data.List (partition)
import Data.List qualified as L
import Marvel.Ability
import Marvel.Card.PlayerCard
import Marvel.Cost
import Marvel.Game.Source
import Marvel.Id
import Marvel.Payment
import Marvel.Resource
import Marvel.Window

data ActiveCostTarget = ForCard PlayerCard | ForAbility Ability | ForTreachery
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON)

data ActiveCost = ActiveCost
  { activeCostIdentityId :: IdentityId
  , activeCostTarget :: ActiveCostTarget
  , activeCostCost :: Cost
  , activeCostPayment :: Payment
  , activeCostWindow :: Maybe Window
  , activeCostSpentCards :: [PlayerCard]
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON)

resourceCostPaid :: MonadGame env m => ActiveCost -> m Bool
resourceCostPaid ActiveCost {..} = do
  let
    (rs, mrs) =
      first catMaybes $ partition isJust (costResources activeCostCost)
  prs <- paymentResources activeCostPayment
  flip evalStateT prs $ do
    l <- fmap and $ for rs $ \r -> do
      prs' <- get
      case (r `elem` prs', Wild `elem` prs') of
        (False, False) -> pure False
        (True, _) -> do
          put $ L.delete r prs'
          pure True
        (_, True) -> do
          put $ L.delete Wild prs'
          pure True
    prs' <- get
    pure $ l && length prs' >= length mrs

