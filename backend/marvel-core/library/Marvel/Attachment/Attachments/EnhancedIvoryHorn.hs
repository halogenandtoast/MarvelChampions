module Marvel.Attachment.Attachments.EnhancedIvoryHorn
  ( enhancedIvoryHorn
  , EnhancedIvoryHorn(..)
  ) where

import Marvel.Prelude

import Marvel.Ability
import Marvel.Attachment.Attrs
import Marvel.Attachment.Cards qualified as Cards
import Marvel.Card.Code
import Marvel.Entity
import Marvel.Message
import Marvel.Source
import Marvel.Target

enhancedIvoryHorn :: AttachmentCard EnhancedIvoryHorn
enhancedIvoryHorn = attachment EnhancedIvoryHorn Cards.enhancedIvoryHorn

newtype EnhancedIvoryHorn = EnhancedIvoryHorn AttachmentAttrs
  deriving anyclass IsAttachment
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, Entity, IsSource, IsTarget)

instance HasAbilities EnhancedIvoryHorn where
  getAbilities _ = []

instance RunMessage EnhancedIvoryHorn where
  runMessage msg (EnhancedIvoryHorn attrs) =
    EnhancedIvoryHorn <$> runMessage msg attrs
