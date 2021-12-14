module Marvel.Source where

import Marvel.Prelude

import Marvel.Id

data Source
  = IdentitySource IdentityId
  | VillainSource VillainId
  | MinionSource MinionId
  | AllySource AllyId
  | EventSource EventId
  | EffectSource EffectId
  | SupportSource SupportId
  | TreacherySource TreacheryId
  | ObligationSource ObligationId
  | SideSchemeSource SideSchemeId
  | MainSchemeSource MainSchemeId
  | AttachmentSource AttachmentId
  | UpgradeSource UpgradeId
  | GameSource
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON, Hashable)

class IsSource a where
  toSource :: a -> Source

instance IsSource Source where
  toSource = id

instance IsSource IdentityId where
  toSource = IdentitySource

instance IsSource VillainId where
  toSource = VillainSource

instance IsSource MinionId where
  toSource = MinionSource

instance IsSource AllyId where
  toSource = AllySource

instance IsSource EventId where
  toSource = EventSource

instance IsSource SupportId where
  toSource = SupportSource

instance IsSource a => IsSource (With a b) where
  toSource (With a _) = toSource a
