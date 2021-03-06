{-# LANGUAGE TemplateHaskell #-}

module Marvel.Attachment where

import Marvel.Prelude

import Marvel.Ability
import Marvel.Attachment.Attachments
import Marvel.Attachment.Attrs
import Marvel.Card
import Marvel.Entity
import Marvel.Id
import Marvel.Message
import Marvel.Modifier
import Marvel.Source
import Marvel.TH
import Marvel.Target

$(buildEntity "Attachment")

allAttachments :: HashMap CardCode (AttachmentId -> Attachment)
allAttachments =
  fromList $
    map
      (toCardCode &&& cbCardBuilder)
      $(buildEntityLookupList "Attachment")

lookupAttachment :: CardCode -> (AttachmentId -> Attachment)
lookupAttachment cardCode = case lookup cardCode allAttachments of
  Just f -> f
  Nothing -> error $ "Invalid card code for attachment " <> show cardCode

instance Entity Attachment where
  type EntityId Attachment = AttachmentId
  type EntityAttrs Attachment = AttachmentAttrs
  toId = toId . toAttrs
  toAttrs = genericToAttrs

instance RunMessage Attachment where
  runMessage = genericRunMessage

instance IsSource Attachment where
  toSource = AttachmentSource . toId

instance IsTarget Attachment where
  toTarget = AttachmentTarget . toId

instance HasAbilities Attachment where
  getAbilities = genericGetAbilities

instance HasModifiersFor Attachment where
  getModifiersFor = genericGetModifiersFor

instance IsCard Attachment where
  toCard = toCard . toAttrs

instance HasCardDef Attachment where
  getCardDef = getCardDef . toAttrs
