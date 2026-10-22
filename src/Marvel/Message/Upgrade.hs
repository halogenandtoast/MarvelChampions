module Marvel.Message.Upgrade where

import Marvel.Prelude

import Marvel.Id
import Marvel.Ref

data UpgradeMessage
  = ExhaustedUpgrade
  | DiscardUpgrade
  | ReadiedUpgrade
  | -- | on what it was played, chosen before its cost was paid
    PlayedUpgrade (Maybe Target)
  | UpgradeControlledByPlayer IdentityId
  | UpgradeAttachedToEnemy EnemyId
  | UpgradeAttachedToAlly AllyId
  | SpendUpgradeUse
  | AddUpgradeUses Natural
  | UpgradeAttachedToSideScheme SideSchemeId
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON)

