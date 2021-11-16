module Marvel.Target where

import Marvel.Prelude

import Marvel.Id
import Marvel.Card.Code

data Target
  = IdentityTarget IdentityId
  | VillainTarget VillainId
  | MinionTarget MinionId
  | AllyTarget AllyId
  | SupportTarget SupportId
  | EventTarget EventId
  | EffectTarget EffectId
  | MainSchemeTarget CardCode
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON)

class IsTarget a where
  toTarget :: a -> Target

instance IsTarget Target where
  toTarget = id
