module Marvel.Attachment.Attachments.MediaCoverage (
  mediaCoverage,
  MediaCoverage (..),
) where

import Marvel.Prelude

import Marvel.Ability
import Marvel.Attachment.Cards qualified as Cards
import Marvel.Attachment.Types
import Marvel.Card.Code
import Marvel.Cost
import Marvel.Entity
import Marvel.Criteria
import Marvel.Message.Lifted
import Marvel.Modifier
import Marvel.Question
import Marvel.Ref
import Marvel.Resource

mediaCoverage :: AttachmentCard MediaCoverage
mediaCoverage = attachment MediaCoverage Cards.mediaCoverage

newtype MediaCoverage = MediaCoverage (Attrs Attachment)
  deriving anyclass (IsAttachment)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

-- | Each "When Revealed" ability you reveal resolves 1 additional time.
instance HasModifiersFor MediaCoverage where
  getModifiersFor _ (IdentityRef ident) (MediaCoverage a) | attachmentIdentity a == Just ident = pure [ResolveWhenRevealedAgain]
  getModifiersFor _ _ _ = pure []

-- | Alter-Ego Action: spend a [mental] resource → discard this card.
instance HasAbilities MediaCoverage where
  getAbilities (MediaCoverage a) = case attachmentIdentity a of
    Just _ -> [ability a 1 AlterEgoAction NoCriteria (MultiResourceCost [Just Mental]) $ DiscardTarget (toRef a)]
    Nothing -> []

-- | Attach to your identity card.
instance RunMessage MediaCoverage where
  runMessage msg a@(MediaCoverage attrs) = runQueueT $ case msg of
    AttachmentMessage ident (RevealAttachment you) | ident == attachmentId attrs -> do
      attachToIdentity ident you
      pure a
    _ -> MediaCoverage <$> runMessage msg attrs
