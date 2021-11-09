module Marvel.Source where

import Marvel.Prelude

import Marvel.Id

data Source
  = IdentitySource IdentityId
  | VillainSource VillainId
  | AllySource AllyId
  | EventSource EventId
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON)

class IsSource a where
  toSource :: a -> Source

instance IsSource Source where
  toSource = id

instance IsSource IdentityId where
  toSource = IdentitySource

instance IsSource VillainId where
  toSource = VillainSource

instance IsSource AllyId where
  toSource = AllySource

instance IsSource EventId where
  toSource = EventSource
