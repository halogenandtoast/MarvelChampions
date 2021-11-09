module Marvel.Id where

import Marvel.Prelude

newtype IdentityId = IdentityId UUID
  deriving newtype (Show, Eq, Random, Hashable, ToJSON, FromJSON, ToJSONKey, FromJSONKey)

newtype VillainId = VillainId UUID
  deriving newtype (Show, Eq, Random, Hashable, ToJSON, FromJSON, ToJSONKey, FromJSONKey)

newtype EnemyId = EnemyVillainId VillainId
  deriving newtype (Show, Eq, Random, Hashable, ToJSON, FromJSON, ToJSONKey, FromJSONKey)

newtype AllyId = AllyId UUID
  deriving newtype (Show, Eq, Random, Hashable, ToJSON, FromJSON, ToJSONKey, FromJSONKey)

newtype EventId = EventId UUID
  deriving newtype (Show, Eq, Random, Hashable, ToJSON, FromJSON, ToJSONKey, FromJSONKey)
