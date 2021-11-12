module Marvel.Id where

import Marvel.Prelude

import Marvel.Card.Code

data CharacterId
  = IdentityCharacter IdentityId
  | AllyCharacter AllyId
  | VillainCharacter VillainId
  | MinionCharacterId MinionId
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON)

newtype IdentityId = IdentityId UUID
  deriving newtype (Show, Eq, Random, Hashable, ToJSON, FromJSON, ToJSONKey, FromJSONKey)

newtype VillainId = VillainId UUID
  deriving newtype (Show, Eq, Random, Hashable, ToJSON, FromJSON, ToJSONKey, FromJSONKey)

newtype MinionId = MinionId UUID
  deriving newtype (Show, Eq, Random, Hashable, ToJSON, FromJSON, ToJSONKey, FromJSONKey)

newtype EnemyId = EnemyVillainId VillainId
  deriving newtype (Show, Eq, Random, Hashable, ToJSON, FromJSON, ToJSONKey, FromJSONKey)

newtype SchemeId = SchemeMainSchemeId CardCode
  deriving newtype (Show, Eq, Hashable, ToJSON, FromJSON, ToJSONKey, FromJSONKey)

newtype AllyId = AllyId { unAllyId :: UUID }
  deriving newtype (Show, Eq, Random, Hashable, ToJSON, FromJSON, ToJSONKey, FromJSONKey)

newtype EventId = EventId { unEventId :: UUID }
  deriving newtype (Show, Eq, Random, Hashable, ToJSON, FromJSON, ToJSONKey, FromJSONKey)
