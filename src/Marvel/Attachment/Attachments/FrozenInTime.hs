module Marvel.Attachment.Attachments.FrozenInTime (
  frozenInTime,
  FrozenInTime (..),
) where

import Marvel.Prelude

import Marvel.Ability
import Marvel.Attachment.Cards qualified as Cards
import Marvel.Attachment.Types
import Marvel.Card.Code
import Marvel.Entity
import Marvel.Game.Source
import Marvel.Identity.Types (Field (PlayerIdentityExhausted))
import Marvel.Message.Lifted
import Marvel.Modifier
import Marvel.Projection
import Marvel.Ref

frozenInTime :: AttachmentCard FrozenInTime
frozenInTime = attachment FrozenInTime Cards.frozenInTime

newtype FrozenInTime = FrozenInTime (Attrs Attachment)
  deriving anyclass (IsAttachment, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

-- | Forced Interrupt: when the attached character would ready, discard this card
-- instead. It stays exhausted while the card is there.
instance HasModifiersFor FrozenInTime where
  getModifiersFor _ (IdentityRef ident) (FrozenInTime a) | attachmentIdentity a == Just ident = pure [CannotReady]
  getModifiersFor _ _ _ = pure []

-- | Attach to your identity. [star] Boost: attach to your identity, the identity of the
-- player the activation is against.
instance RunMessage FrozenInTime where
  runMessage msg a@(FrozenInTime attrs) = runQueueT $ case msg of
    AttachmentMessage ident (RevealAttachment you) | ident == attachmentId attrs -> do
      attachToIdentity ident you
      pure a
    Boost (RevealedAsBoost target enemy) | isTarget attrs target -> do
      you <- getBoostedPlayer enemy
      putBoostIntoPlay attrs you
      attachToIdentity (attachmentId attrs) you
      pure a
    -- the identity readies with its other cards, or by an ability: exhausted, it would
    -- ready, and the card is discarded instead
    IdentityMessage ident step
      | Just ident == attachmentIdentity attrs
      , step `elem` [ReadyCards, ReadiedIdentity] -> do
          exhausted <- project PlayerIdentityExhausted ident
          when exhausted $ removeFromPlay attrs
          pure a
    _ -> FrozenInTime <$> runMessage msg attrs
