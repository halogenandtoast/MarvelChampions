module Marvel.Event.Events.SelfPropelledGlide (
  selfPropelledGlide,
  selfPropelledGlideEffect,
  SelfPropelledGlide (..),
  SelfPropelledGlideEffect (..),
) where

import Marvel.Prelude

import Marvel.Card.Code
import Marvel.Damage ()
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
import Marvel.Trait (Trait (Aerial))

selfPropelledGlide :: EventCard SelfPropelledGlide
selfPropelledGlide = event SelfPropelledGlide Cards.selfPropelledGlide

newtype SelfPropelledGlide = SelfPropelledGlide (Attrs Event)
  deriving anyclass (IsEvent, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

-- | Ready Spider-Woman. She gains Aerial until the end of the round.
instance RunMessage SelfPropelledGlide where
  runMessage msg e@(SelfPropelledGlide attrs) = runQueueT $ case msg of
    EventMessage ident (PlayedEvent identityId _ _) | ident == eventId attrs -> do
      ready identityId
      createEffect Cards.selfPropelledGlide attrs (IdentityEntity $ IdentityWithId identityId)
      pure e
    _ -> SelfPropelledGlide <$> runMessage msg attrs

-- | Aerial until the end of the round.
newtype SelfPropelledGlideEffect = SelfPropelledGlideEffect (Attrs Effect)
  deriving anyclass (IsEffect)
  deriving newtype (Show, Eq, ToJSON, FromJSON, IsRef)

selfPropelledGlideEffect :: CardEffect SelfPropelledGlideEffect
selfPropelledGlideEffect =
  effectWith
    SelfPropelledGlideEffect
    Cards.selfPropelledGlide
    ((modifiersL .~ [TraitModifier Aerial]) . (endsL ?~ DisableAtEndOfRound))

instance RunMessage SelfPropelledGlideEffect where
  runMessage msg (SelfPropelledGlideEffect attrs) = SelfPropelledGlideEffect <$> runMessage msg attrs
