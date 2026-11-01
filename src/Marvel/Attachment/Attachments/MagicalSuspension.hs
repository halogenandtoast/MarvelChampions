module Marvel.Attachment.Attachments.MagicalSuspension (
  magicalSuspension,
  MagicalSuspension (..),
) where

import Marvel.Prelude

import Marvel.Ability
import Marvel.Attachment.Cards qualified as Cards
import Marvel.Attachment.Types
import Marvel.Card.Code
import Marvel.Card.Def (CardMatcher (AnyCard))
import Marvel.Cost
import Marvel.Criteria
import Marvel.Entity ()
import Marvel.Message.Lifted
import Marvel.Modifier
import Marvel.Question
import Marvel.Ref

magicalSuspension :: AttachmentCard MagicalSuspension
magicalSuspension = attachment MagicalSuspension Cards.magicalSuspension

newtype MagicalSuspension = MagicalSuspension (Attrs Attachment)
  deriving anyclass (IsAttachment)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

-- | Each card you play costs 1 additional resource.
instance HasModifiersFor MagicalSuspension where
  getModifiersFor _ (IdentityRef ident) (MagicalSuspension a)
    | attachmentIdentity a == Just ident = pure [AdditionalResourceCost AnyCard 1]
  getModifiersFor _ _ _ = pure []

-- | Hero Action: exhaust your hero → discard this card. Only the player it is attached
-- to may take it (RR Ability: an attachment saying "your"), as the engine's controller
-- of an attachment's abilities is its identity.
instance HasAbilities MagicalSuspension where
  getAbilities (MagicalSuspension a) = case attachmentIdentity a of
    Just _ -> [ability a 1 HeroAction NoCriteria ExhaustCost $ DiscardTarget (toRef a)]
    Nothing -> []

-- | Attach to your identity.
instance RunMessage MagicalSuspension where
  runMessage msg a@(MagicalSuspension attrs) = runQueueT $ case msg of
    AttachmentMessage ident (RevealAttachment you) | ident == attachmentId attrs -> do
      attachToIdentity ident you
      pure a
    _ -> MagicalSuspension <$> runMessage msg attrs
