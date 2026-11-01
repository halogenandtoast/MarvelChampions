module Marvel.Event.Events.Moxie (
  moxie,
  Moxie (..),
  moxieEffect,
  MoxieEffect (..),
) where

import Marvel.Prelude

import Marvel.Card.Code
import Marvel.Entity
import Marvel.Event.Cards qualified as Cards
import Marvel.Event.Types
import Marvel.Id ()
import Marvel.Matchers
import Marvel.Message.Lifted
import Marvel.Modifier
import Marvel.Query ()
import Marvel.Ref
import Marvel.Effect.Types

moxie :: EventCard Moxie
moxie = event Moxie Cards.moxie

newtype Moxie = Moxie (Attrs Event)
  deriving anyclass (IsEvent, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

-- | Hero Response: After you change form, your hero gets +1 THW, +1 ATK, +1 DEF until the
-- end of the round.
instance RunMessage Moxie where
  runMessage msg e@(Moxie attrs) = runQueueT $ case msg of
    EventMessage ident (PlayedEvent you _ _) | ident == eventId attrs -> do
      createEffect Cards.moxie attrs (IdentityEntity $ IdentityWithId you)
      pure e
    _ -> Moxie <$> runMessage msg attrs

newtype MoxieEffect = MoxieEffect (Attrs Effect)
  deriving anyclass (IsEffect)
  deriving newtype (Show, Eq, ToJSON, FromJSON, IsRef)

moxieEffect :: CardEffect MoxieEffect
moxieEffect =
  effectWith MoxieEffect Cards.moxie $
    (modifiersL .~ [ThwartModifier 1, AttackModifier 1, DefenseModifier 1])
      . (endsL ?~ DisableAtEndOfRound)

instance RunMessage MoxieEffect where
  runMessage msg (MoxieEffect attrs) = MoxieEffect <$> runMessage msg attrs
