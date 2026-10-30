module Marvel.Attachment.Attachments.InABind (
  inABind,
  InABind (..),
) where

import Marvel.Prelude

import Marvel.Ability
import Marvel.Attachment.Cards qualified as Cards
import Marvel.Attachment.Types
import Marvel.Card
import Marvel.Card.Code
import Marvel.Cost
import Marvel.Criteria
import Marvel.Damage
import Marvel.Entity
import Marvel.Game.Source
import Marvel.Hand
import Marvel.Identity.Types (Field (PlayerIdentityHand))
import Marvel.Matchers
import Marvel.Message.Lifted
import Marvel.Modifier
import Marvel.Projection
import Marvel.Query
import Marvel.Question (runAbility)
import Marvel.Ref
import Marvel.Trait qualified as Trait

inABind :: AttachmentCard InABind
inABind = attachment InABind Cards.inABind

newtype InABind = InABind (Attrs Attachment)
  deriving anyclass (IsAttachment)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

attackEvent :: CardMatcher
attackEvent = CardWithType EventType <> CardWithTrait Trait.Attack

-- | Treat Gamora's printed text box as if it were blank (except for traits): her basic
-- powers and changing form are the game's, and stay.
instance HasModifiersFor InABind where
  getBlankingModifiersFor _ (IdentityRef ident) (InABind a) | attachmentIdentity a == Just ident = pure [Blank]
  getBlankingModifiersFor _ _ _ = pure []

-- | Hero Action: choose and discard an [attack] event from your hand and deal 1 damage to
-- Gamora → discard this card.
instance HasAbilities InABind where
  getAbilities (InABind a) = case attachmentIdentity a of
    Just _ -> [ability a 1 HeroAction (ExtendedCardExists $ InHandOf You $ BasicCardMatches attackEvent) NoCost $ runAbility a 1]
    Nothing -> []

-- | Attach to Gamora.
instance RunMessage InABind where
  runMessage msg a@(InABind attrs) = runQueueT $ case msg of
    AttachmentMessage ident (RevealAttachment _) | ident == attachmentId attrs -> do
      selectOne (IdentityWithTitle "Gamora") >>= \case
        Just gamora -> attachToIdentity ident gamora
        Nothing -> removeFromPlay attrs
      pure a
    RanAbility you (isTarget attrs -> True) 1 _ _ -> do
      hand <- filter (cardMatch attackEvent) . unHand <$> project PlayerIdentityHand you
      unless (null hand) do
        chooseOne you [targeting c (discardCard c) | c <- hand]
        for_ (attachmentIdentity attrs) \gamora -> damageIdentity gamora attrs (toDamage 1 FromAbility)
        removeFromPlay attrs
      pure a
    _ -> InABind <$> runMessage msg attrs
