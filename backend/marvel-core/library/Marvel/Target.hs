module Marvel.Target where

import Marvel.Prelude

import Marvel.Card.Code
import Marvel.Card.Id
import Marvel.Id

data Target
  = IdentityTarget IdentityId
  | VillainTarget VillainId
  | MinionTarget MinionId
  | EnemyTarget EnemyId
  | AllyTarget AllyId
  | SupportTarget SupportId
  | EventTarget EventId
  | EffectTarget EffectId
  | TreacheryTarget TreacheryId
  | UpgradeTarget UpgradeId
  | SideSchemeTarget SideSchemeId
  | AttachmentTarget AttachmentId
  | MainSchemeTarget CardCode
  | SchemeTarget SchemeId
  | CardIdTarget CardId
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON)

class IsTarget a where
  toTarget :: a -> Target

instance IsTarget Target where
  toTarget = id
