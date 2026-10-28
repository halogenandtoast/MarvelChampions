-- | The attachment of the Frost Giants modular set (The Mad Titan's Shadow).
module Marvel.Attachment.Attachments.FrostGiants (
  frozen,
  Frozen (..),
) where

import Marvel.Prelude

import Marvel.Ability
import Marvel.Attachment.Cards qualified as Cards
import Marvel.Attachment.Types
import Marvel.Card.Code
import Marvel.Cost
import Marvel.Criteria
import Marvel.Entity
import Marvel.Message.Lifted
import Marvel.Modifier
import Marvel.Question
import Marvel.Ref
import Marvel.Resource

frozen :: AttachmentCard Frozen
frozen = attachment Frozen Cards.frozen

newtype Frozen = Frozen (Attrs Attachment)
  deriving anyclass (IsAttachment)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

-- | Attached identity cannot ready, by any means.
instance HasModifiersFor Frozen where
  getModifiersFor _ (IdentityRef ident) (Frozen a) | attachmentIdentity a == Just ident = pure [CannotReady]
  getModifiersFor _ _ _ = pure []

-- | Alter-Ego Action: spend [energy] [physical] resources → discard this card.
instance HasAbilities Frozen where
  getAbilities (Frozen a) = case attachmentIdentity a of
    Just _ -> [ability a 1 AlterEgoAction NoCriteria (MultiResourceCost [Just Energy, Just Physical]) $ DiscardTarget (toRef a)]
    Nothing -> []

-- | Attach to your identity.
instance RunMessage Frozen where
  runMessage msg a@(Frozen attrs) = runQueueT $ case msg of
    AttachmentMessage ident (RevealAttachment you) | ident == attachmentId attrs -> do
      attachToIdentity ident you
      pure a
    _ -> Frozen <$> runMessage msg attrs
