module Marvel.Attachment where

import Marvel.Prelude

import Data.Typeable
import Marvel.Ability
import Marvel.Attachment.Attachments
import Marvel.Attachment.Attrs
import Marvel.Card
import Marvel.Entity
import Marvel.Id
import Marvel.Message
import Marvel.Modifier
import Marvel.Source
import Marvel.Target
import Text.Show qualified

data Attachment = forall a. IsAttachment a => Attachment a

instance Show Attachment where
  show (Attachment a) = show a

instance ToJSON Attachment where
  toJSON (Attachment a) = toJSON a

instance Eq Attachment where
  (Attachment (a :: a)) == (Attachment (b :: b)) = case eqT @a @b of
    Just Refl -> a == b
    Nothing -> False

instance FromJSON Attachment where
  parseJSON v = flip (withObject "Attachment") v $ \o -> do
    cardDef <- o .: "attachmentCardDef"
    withAttachmentCardCode (cdCardCode cardDef) $ \(_ :: AttachmentCard a) -> Attachment <$> parseJSON @a v

withAttachmentCardCode
  :: CardCode
  -> (forall a. IsAttachment a => AttachmentCard a -> r)
  -> r
withAttachmentCardCode cCode f =
  case lookup cCode allAttachments of
    Nothing -> error "invalid attachment"
    Just (SomeAttachmentCard a) -> f a

data SomeAttachmentCard = forall a. IsAttachment a => SomeAttachmentCard (AttachmentCard a)

liftAttachmentCard :: (forall a . AttachmentCard a -> b) -> SomeAttachmentCard -> b
liftAttachmentCard f (SomeAttachmentCard a) = f a

someAttachmentCardCode :: SomeAttachmentCard -> CardCode
someAttachmentCardCode = liftAttachmentCard cbCardCode

allAttachments :: HashMap CardCode SomeAttachmentCard
allAttachments =
  fromList $
    map
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

instance Entity Attachment where
  type EntityId Attachment = AttachmentId
  type EntityAttrs Attachment = AttachmentAttrs
  toId = toId . toAttrs
  toAttrs (Attachment a) = toAttrs a

instance RunMessage Attachment where
  runMessage msg (Attachment a) = Attachment <$> runMessage msg a

instance IsSource Attachment where
  toSource = AttachmentSource . toId

instance IsTarget Attachment where
  toTarget = AttachmentTarget . toId

instance HasAbilities Attachment where
  getAbilities (Attachment a) = getAbilities a

instance HasModifiersFor Attachment where
  getModifiersFor source target (Attachment a) = getModifiersFor source target a

instance IsCard Attachment where
  toCard = toCard . toAttrs

instance HasCardDef Attachment where
  getCardDef = getCardDef . toAttrs
