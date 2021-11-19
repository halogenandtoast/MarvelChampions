{-# LANGUAGE TemplateHaskell #-}
module Marvel.Attachment.Attrs where

import Marvel.Prelude

import Marvel.Card.Builder
import Marvel.Card.Code
import Marvel.Card.Def
import Marvel.Entity
import Marvel.Id
import Marvel.Message
import Marvel.Source
import Marvel.Target

class IsAttachment a

type AttachmentCard a = CardBuilder AttachmentId a

data AttachmentAttrs = AttachmentAttrs
  { attachmentId :: AttachmentId
  , attachmentCardDef :: CardDef
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON)

makeLensesWith suffixedFields ''AttachmentAttrs

instance HasCardCode AttachmentAttrs where
  toCardCode = toCardCode . attachmentCardDef

attachment :: (AttachmentAttrs -> a) -> CardDef -> CardBuilder AttachmentId a
attachment f cardDef = CardBuilder
  { cbCardCode = cdCardCode cardDef
  , cbCardBuilder = \mid ->
    f $ AttachmentAttrs { attachmentId = mid, attachmentCardDef = cardDef }
  }

instance Entity AttachmentAttrs where
  type EntityId AttachmentAttrs = AttachmentId
  type EntityAttrs AttachmentAttrs = AttachmentAttrs
  toId = attachmentId
  toAttrs = id

instance IsSource AttachmentAttrs where
  toSource = AttachmentSource . toId

instance IsTarget AttachmentAttrs where
  toTarget = AttachmentTarget . toId

isTarget :: (Entity a, EntityAttrs a ~ AttachmentAttrs) => a -> Target -> Bool
isTarget a = (== toTarget (toAttrs a))

instance RunMessage AttachmentAttrs where
  runMessage _ = pure
