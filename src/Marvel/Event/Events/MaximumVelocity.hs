module Marvel.Event.Events.MaximumVelocity (
  maximumVelocity,
  MaximumVelocity (..),
  maximumVelocityEffect,
  MaximumVelocityEffect (..),
) where

import Marvel.Prelude

import Marvel.Card.Code
import Marvel.Entity
import Marvel.Event.Cards qualified as Cards
import Marvel.Effect.Types
import Marvel.Event.Types
import Marvel.Matchers
import Marvel.Message.Lifted
import Marvel.Modifier
import Marvel.Ref

maximumVelocity :: EventCard MaximumVelocity
maximumVelocity = event MaximumVelocity Cards.maximumVelocity

newtype MaximumVelocity = MaximumVelocity (Attrs Event)
  deriving anyclass (IsEvent, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

-- | Max 1 per phase (its card limit). Hero Action: You get +2 THW, +2 ATK, and +2 DEF
-- until the end of the round.
instance RunMessage MaximumVelocity where
  runMessage msg e@(MaximumVelocity attrs) = runQueueT $ case msg of
    EventMessage ident (PlayedEvent you _ _) | ident == eventId attrs -> do
      createEffect Cards.maximumVelocity attrs (IdentityEntity $ IdentityWithId you)
      pure e
    _ -> MaximumVelocity <$> runMessage msg attrs

newtype MaximumVelocityEffect = MaximumVelocityEffect (Attrs Effect)
  deriving anyclass (IsEffect)
  deriving newtype (Show, Eq, ToJSON, FromJSON, IsRef)

maximumVelocityEffect :: CardEffect MaximumVelocityEffect
maximumVelocityEffect =
  effectWith MaximumVelocityEffect Cards.maximumVelocity $
    (modifiersL .~ [ThwartModifier 2, AttackModifier 2, DefenseModifier 2])
      . (endsL ?~ DisableAtEndOfRound)

instance RunMessage MaximumVelocityEffect where
  runMessage msg (MaximumVelocityEffect attrs) = MaximumVelocityEffect <$> runMessage msg attrs
