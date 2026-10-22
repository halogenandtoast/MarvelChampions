module Marvel.Message.Upgrade where

import Marvel.Prelude

import Marvel.Id

data UpgradeMessage
  = ExhaustedUpgrade
  | DiscardUpgrade
  | ReadiedUpgrade
  | PlayedUpgrade
  | UpgradeControlledByPlayer IdentityId
  | UpgradeAttachedToEnemy EnemyId
  | UpgradeAttachedToAlly AllyId
  | SpendUpgradeUse
  | AddUpgradeUses Natural
  | UpgradeAttachedToSideScheme SideSchemeId
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON)

