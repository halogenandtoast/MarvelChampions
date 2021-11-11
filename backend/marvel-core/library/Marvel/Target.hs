module Marvel.Target where

import Marvel.Prelude

import Marvel.Id
import Marvel.Card.Code

data Target
  = IdentityTarget IdentityId
  | VillainTarget VillainId
  | AllyTarget AllyId
  | EventTarget EventId
  | MainSchemeTarget CardCode
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON)

class IsTarget a where
  toTarget :: a -> Target

instance IsTarget Target where
  toTarget = id
