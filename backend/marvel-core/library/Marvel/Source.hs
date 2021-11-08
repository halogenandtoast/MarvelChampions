module Marvel.Source where

import Marvel.Prelude

import Marvel.Id

data Source = IdentitySource IdentityId | VillainSource VillainId | AllySource AllyId
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON)

class IsSource a where
  toSource :: a -> Source

instance IsSource Source where
  toSource = id
