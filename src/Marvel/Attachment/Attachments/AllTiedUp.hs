module Marvel.Attachment.Attachments.AllTiedUp (
  allTiedUp,
  AllTiedUp (..),
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

allTiedUp :: AttachmentCard AllTiedUp
allTiedUp = attachment AllTiedUp Cards.allTiedUp

newtype AllTiedUp = AllTiedUp (Attrs Attachment)
  deriving anyclass (IsAttachment)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

-- | The attached character cannot ready or change form.
instance HasModifiersFor AllTiedUp where
  getModifiersFor _ (IdentityRef ident) (AllTiedUp a) | attachmentIdentity a == Just ident = pure [CannotReady, CannotChangeForm]
  getModifiersFor _ _ _ = pure []

-- | Action: spend [mental] [physical] resources → discard this card.
instance HasAbilities AllTiedUp where
  getAbilities (AllTiedUp a) = case attachmentIdentity a of
    Just _ -> [ability a 1 Action NoCriteria (MultiResourceCost [Just Mental, Just Physical]) $ DiscardTarget (toRef a)]
    Nothing -> []

-- | Attach to your identity card.
instance RunMessage AllTiedUp where
  runMessage msg a@(AllTiedUp attrs) = runQueueT $ case msg of
    AttachmentMessage ident (RevealAttachment you) | ident == attachmentId attrs -> do
      attachToIdentity ident you
      pure a
    _ -> AllTiedUp <$> runMessage msg attrs
