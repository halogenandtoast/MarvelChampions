module Marvel.Target where

import Marvel.Prelude

import Marvel.Card.Code
import Marvel.Card.Id
import Marvel.Entity
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
  | MainSchemeTarget CardCode
  | SchemeTarget SchemeId
  | CardIdTarget CardId
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON)

class IsTarget a where
  toTarget :: a -> Target

instance IsTarget Target where
  toTarget = id

isTarget :: (Entity a, EntityAttrs a ~ b, IsTarget b) => a -> Target -> Bool
isTarget a = (== toTarget (toAttrs a))

instance IsTarget a => IsTarget (With a b) where
  toTarget (With a _) = toTarget a
