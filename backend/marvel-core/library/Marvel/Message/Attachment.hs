module Marvel.Message.Attachment where

import Marvel.Prelude

import Marvel.Id

data AttachmentMessage
  = RevealAttachment IdentityId
  | AttachmentDamaged Natural
  | AttachedToEnemy EnemyId
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON)
