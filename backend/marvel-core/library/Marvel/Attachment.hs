{-# OPTIONS_GHC -Wno-orphans #-}
module Marvel.Attachment where

import Marvel.Prelude

import Marvel.Attachment.Attachments
import Marvel.Attachment.Types
import Marvel.Card
import Marvel.Id

instance FromJSON Attachment where
  parseJSON = withObject "Attachment" $ \o -> do
    cardDef <- o .: "attachmentCardDef"
    withAttachmentCardCode (cdCardCode cardDef)
      $ \(_ :: AttachmentCard a) -> Attachment <$> parseJSON @a (Object o)

withAttachmentCardCode
  :: CardCode -> (forall a . IsAttachment a => AttachmentCard a -> r) -> r
withAttachmentCardCode cCode f = case lookup cCode allAttachments of
  Nothing -> error "invalid attachment"
  Just (SomeAttachmentCard a) -> f a

allAttachments :: HashMap CardCode SomeAttachmentCard
allAttachments = fromList $ map
  (toFst someAttachmentCardCode)
  [ SomeAttachmentCard armoredRhinoSuit
  , SomeAttachmentCard charge
  , SomeAttachmentCard enhancedIvoryHorn
  , SomeAttachmentCard sonicConverter
  , SomeAttachmentCard solidSoundBody
      -- , SomeAttachmentCard programTransmitter
      -- , SomeAttachmentCard upgradedDrones
      -- , SomeAttachmentCard vibraniumArmor
      -- , SomeAttachmentCard concussionBlasters
  , SomeAttachmentCard geneticallyEnhanced
      -- , SomeAttachmentCard biomechanicalUpgrades
  ]

lookupAttachment :: CardCode -> AttachmentId -> Attachment
lookupAttachment cardCode = case lookup cardCode allAttachments of
  Just (SomeAttachmentCard a) -> Attachment <$> cbCardBuilder a
  Nothing -> error $ "Invalid card code for attachment " <> show cardCode
