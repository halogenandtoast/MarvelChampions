module Marvel.ActiveCost.Types where

import Marvel.Prelude

import Marvel.Ability.Types
import Marvel.Card.PlayerCard.Types
import Marvel.Cost.Types
import Marvel.Id
import Marvel.Payment.Types
import Marvel.Window.Types

data ActiveCostTarget
  = ForCard PlayerCard
  | ForAbility Ability
  | ForTreachery
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON)

data ActiveCost = ActiveCost
  { activeCostId :: ActiveCostId
  , activeCostIdentityId :: IdentityId
  , activeCostTarget :: ActiveCostTarget
  , activeCostCost :: Cost
  , activeCostPayment :: Payment
  , activeCostWindow :: Maybe Window
  , activeCostSpentCards :: [PlayerCard]
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON)
