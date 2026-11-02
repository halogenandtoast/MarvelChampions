module Marvel.Message.Attachment where

import Marvel.Prelude

import Marvel.Id

data AttachmentMessage
  = RevealAttachment IdentityId
  | ExhaustedAttachment
  | ReadiedAttachment
  | AttachmentDamaged Natural
  | AttachedToEnemy EnemyId
  | -- | attached to the player's identity card ("attach to your identity")
    AttachedToIdentity IdentityId
  | AttachedToSideScheme SideSchemeId
  | -- | attached to a main scheme (Tower Defense's Focused Defense)
    AttachedToMainScheme MainSchemeId
  | -- | attached to a player's ally (Ebony Maw's Restrained)
    AttachedToAlly AllyId
  | AttachmentHostBecameAlly AllyId
  | AttachmentHostBecameMinion MinionId
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON)
