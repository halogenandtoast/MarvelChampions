module Marvel.Attachment.Attachments.Counterspell (
  counterspell,
  Counterspell (..),
) where

import Marvel.Prelude

import Marvel.Ability
import Marvel.Attachment.Cards qualified as Cards
import Marvel.Attachment.Types
import Marvel.Card
import Marvel.Cost
import Marvel.Entity
import Marvel.Matchers
import Marvel.Message
import Marvel.Modifier
import Marvel.Query
import Marvel.Question
import Marvel.Queue
import Marvel.Ref
import Marvel.Window qualified as W

counterspell :: AttachmentCard Counterspell
counterspell = attachment Counterspell Cards.counterspell

newtype Counterspell = Counterspell (Attrs Attachment)
  deriving anyclass (IsAttachment, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

instance HasAbilities Counterspell where
  getAbilities (Counterspell a) = case attachmentIdentity a of
    Just ident ->
      [ windowAbility a 1 (W.PlaysEvent W.When (IdentityWithId ident) AnyCard) ForcedInterrupt NoCost $
          runAbility a 1
      ]
    Nothing -> []

-- | Attach to your hero: an alter-ego has none, so it is discarded (FAQ), no other card
-- revealed in its place. When you play an event, cancel its effects (it is still played
-- and discarded), then discard this card.
instance RunMessage Counterspell where
  runMessage msg a@(Counterspell attrs) = case msg of
    AttachmentMessage ident (RevealAttachment identityId) | ident == attachmentId attrs -> do
      hero <- member identityId <$> select HeroIdentity
      if hero
        then pure . Counterspell $ attrs {attachmentIdentity = Just identityId}
        else a <$ push (RemoveFromPlay $ toTarget attrs)
    RanAbility _ (isTarget attrs -> True) 1 windows _ -> do
      for_ [eid | W.EventPlayed _ eid _ <- windows] \eid ->
        cancelMatchingMessage \case
          EventMessage eid' (PlayedEvent {}) -> eid' == eid
          _ -> False
      push $ RemoveFromPlay (toTarget attrs)
      pure a
    _ -> Counterspell <$> runMessage msg attrs
