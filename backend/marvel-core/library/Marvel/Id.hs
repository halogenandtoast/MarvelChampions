module Marvel.Id where

import Marvel.Prelude

data CharacterId
  = IdentityCharacter IdentityId
  | AllyCharacter AllyId
  | VillainCharacter VillainId
  | MinionCharacter MinionId
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON, Hashable)

newtype IdentityId = IdentityId UUID
  deriving newtype (Show, Eq, Random, Hashable, ToJSON, FromJSON, ToJSONKey, FromJSONKey)

newtype VillainId = VillainId UUID
  deriving newtype (Show, Eq, Random, Hashable, ToJSON, FromJSON, ToJSONKey, FromJSONKey)

newtype MinionId = MinionId {unMinionId :: UUID}
  deriving newtype (Show, Eq, Random, Hashable, ToJSON, FromJSON, ToJSONKey, FromJSONKey)

data EnemyId = EnemyVillainId VillainId | EnemyMinionId MinionId
  deriving stock (Show, Eq, Generic)
  deriving anyclass (Hashable, ToJSON, FromJSON, ToJSONKey, FromJSONKey)

data SchemeId = SchemeMainSchemeId MainSchemeId | SchemeSideSchemeId SideSchemeId
  deriving stock (Show, Eq, Generic)
  deriving anyclass (Hashable, ToJSON, FromJSON, ToJSONKey, FromJSONKey)

newtype AllyId = AllyId {unAllyId :: UUID}
  deriving newtype (Show, Eq, Random, Hashable, ToJSON, FromJSON, ToJSONKey, FromJSONKey)

newtype SupportId = SupportId {unSupportId :: UUID}
  deriving newtype (Show, Eq, Random, Hashable, ToJSON, FromJSON, ToJSONKey, FromJSONKey)

newtype UpgradeId = UpgradeId {unUpgradeId :: UUID}
  deriving newtype (Show, Eq, Random, Hashable, ToJSON, FromJSON, ToJSONKey, FromJSONKey)

newtype TreacheryId = TreacheryId {unTreacheryId :: UUID}
  deriving newtype (Show, Eq, Random, Hashable, ToJSON, FromJSON, ToJSONKey, FromJSONKey)

newtype ObligationId = ObligationId {unObligationId :: UUID}
  deriving newtype (Show, Eq, Random, Hashable, ToJSON, FromJSON, ToJSONKey, FromJSONKey)

newtype MainSchemeId = MainSchemeId {unMainSchemeId :: UUID}
  deriving newtype (Show, Eq, Random, Hashable, ToJSON, FromJSON, ToJSONKey, FromJSONKey)

newtype SideSchemeId = SideSchemeId {unSideSchemeId :: UUID}
  deriving newtype (Show, Eq, Random, Hashable, ToJSON, FromJSON, ToJSONKey, FromJSONKey)

newtype AttachmentId = AttachmentId {unAttachmentId :: UUID}
  deriving newtype (Show, Eq, Random, Hashable, ToJSON, FromJSON, ToJSONKey, FromJSONKey)

newtype EventId = EventId {unEventId :: UUID}
  deriving newtype (Show, Eq, Random, Hashable, ToJSON, FromJSON, ToJSONKey, FromJSONKey)

newtype EffectId = EffectId {unEffectId :: UUID}
  deriving newtype (Show, Eq, Random, Hashable, ToJSON, FromJSON, ToJSONKey, FromJSONKey)
