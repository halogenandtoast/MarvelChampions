module Marvel.Message.Attachment where

import Marvel.Prelude

import Marvel.Id

data AttachmentMessage
  = RevealAttachment IdentityId
  | AttachmentDamaged Natural
  | AttachedToEnemy EnemyId
  | -- | attached to the player's identity card ("attach to your identity")
    AttachedToIdentity IdentityId
  | AttachedToSideScheme SideSchemeId
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON)
