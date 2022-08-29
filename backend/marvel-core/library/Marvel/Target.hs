module Marvel.Target where

import Marvel.Prelude

import Marvel.Card.Id
import Marvel.Id

data Target
  = IdentityTarget IdentityId
  | VillainTarget VillainId
  | MinionTarget MinionId
  | EnemyTarget EnemyId
  | AllyTarget AllyId
  | CharacterTarget CharacterId
  | SupportTarget SupportId
  | EventTarget EventId
  | EffectTarget EffectId
  | TreacheryTarget TreacheryId
  | ObligationTarget ObligationId
  | UpgradeTarget UpgradeId
  | SideSchemeTarget SideSchemeId
  | AttachmentTarget AttachmentId
  | MainSchemeTarget MainSchemeId
  | SchemeTarget SchemeId
  | CardIdTarget CardId
  | ActiveCostTarget ActiveCostId
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON, Hashable, ToJSONKey, FromJSONKey)

class IsTarget a where
  toTarget :: a -> Target

instance IsTarget Target where
  toTarget = id

instance IsTarget a => IsTarget (With a b) where
  toTarget (With a _) = toTarget a
