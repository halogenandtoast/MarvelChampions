module Marvel.Source where

import Marvel.Id

data Source = IdentitySource IdentityId | VillainSource VillainId

class IsSource a where
  toSource :: a -> Source
