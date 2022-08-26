module Marvel.ActiveCost.Types where

import Marvel.Prelude

import Marvel.Ability
import Marvel.Card.PlayerCard
import Marvel.Cost
import Marvel.Id
import Marvel.Payment
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
