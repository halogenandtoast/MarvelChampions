module Marvel.Id where

import Marvel.Prelude

newtype IdentityId = IdentityId UUID
  deriving newtype (Show, Eq, Random, Hashable, ToJSON, FromJSON)

newtype VillainId = VillainId UUID
  deriving newtype (Show, Eq, Random, Hashable, ToJSON, FromJSON)
