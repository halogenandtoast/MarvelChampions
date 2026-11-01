module Marvel.ActiveCost.Types where

import Marvel.Prelude
import Data.Aeson.KeyMap qualified as KeyMap

import Marvel.Ability.Types
import Marvel.Card.PlayerCard.Types
import Marvel.Cost.Types
import Marvel.Id
import Marvel.Payment.Types
import {-# SOURCE #-} Marvel.Message (Message)
import Marvel.Ref (Source, Target)
import Marvel.Window.Types

data ActiveCostTarget
  = ForCard PlayerCard
  | ForAbility Ability
  | ForTreachery
  | ForContinuation Source [Message]
  | ForPaymentContinuation Source Target Text Value
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
  , activeCostAbilityWindows :: Maybe [WindowType]
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON)

instance FromJSON ActiveCost where
  parseJSON = withObject "ActiveCost" $ \o ->
    genericParseJSON defaultOptions $ Object $
      KeyMap.insertWith (\_ existing -> existing) "activeCostAbilityWindows" Null o
