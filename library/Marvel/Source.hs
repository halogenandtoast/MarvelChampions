module Marvel.Source where

import Marvel.Prelude

import Marvel.Id

data Source = IdentitySource IdentityId | VillainSource VillainId
  deriving stock Show

class IsSource a where
  toSource :: a -> Source

instance IsSource Source where
  toSource = id
