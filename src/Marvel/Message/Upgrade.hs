module Marvel.Message.Upgrade where

import Marvel.Prelude

import Marvel.Id
import Marvel.Ref

data UpgradeMessage
  = UpgradeDetached
  | ExhaustedUpgrade
  | -- | whose deck it came from, and whose discard pile it goes to
    UpgradeOwnedBy IdentityId
  | DiscardUpgrade
  | ReadiedUpgrade
  | -- | on what it was played, chosen before its cost was paid
    PlayedUpgrade (Maybe Target)
  | UpgradeControlledByPlayer IdentityId
  | UpgradeAttachedToTarget Target
  | UpgradeAttachedToEnemy EnemyId
  | UpgradeAttachedToIdentity IdentityId
  | UpgradeAttachedToAlly AllyId
  | -- | Same physical host, now treated as a minion; preserve its ally-reference alias.
    UpgradeHostBecameMinion MinionId
  | SpendUpgradeUse
  | AddUpgradeUses Natural
  | UpgradeAttachedToSideScheme SideSchemeId
  | UpgradeAttachedToMainScheme MainSchemeId
  | -- | an energy form turned faceup: the form its identity is in
    FlipUpgradeFaceUp
  | FlipUpgradeFaceDown
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON)

