module Marvel.Message.Upgrade where

import Marvel.Prelude

import Marvel.Id

data UpgradeMessage
  = ExhaustedUpgrade
  | DiscardUpgrade
  | ReadiedUpgrade
  | PlayedUpgrade
  | UpgradeAttachedToEnemy EnemyId
  | UpgradeAttachedToAlly AllyId
  | SpendUpgradeUse
  | AddUpgradeUses Natural
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON)

