module Marvel.Event.Events.IveGotAPlan (
  iveGotAPlan,
  IveGotAPlan (..),
  iveGotAPlanEffect,
  IveGotAPlanEffect (..),
) where

import Marvel.Prelude

import Marvel.Card.Code
import Marvel.Effect.Types
import Marvel.Event.Cards qualified as Cards
import Marvel.Event.Types
import Marvel.Matchers
import Marvel.Message.Lifted
import Marvel.Modifier
import Marvel.Ref

iveGotAPlan :: EventCard IveGotAPlan
iveGotAPlan = event IveGotAPlan Cards.iveGotAPlan

newtype IveGotAPlan = IveGotAPlan (Attrs Event)
  deriving anyclass (IsEvent, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

-- | Hero Response: After you make a basic thwart, ready Rocket Raccoon. Rocket Raccoon
-- gets +1 THW while in hero form until the end of the phase: a lasting effect on the
-- identity while a hero, so it is lost in alter-ego form and back if he flips again.
instance RunMessage IveGotAPlan where
  runMessage msg e@(IveGotAPlan attrs) = runQueueT $ case msg of
    EventMessage ident (PlayedEvent you _ _) | ident == eventId attrs -> do
      ready you
      createEffect Cards.iveGotAPlan attrs (IdentityEntity $ IdentityWithId you <> HeroIdentity)
      pure e
    _ -> IveGotAPlan <$> runMessage msg attrs

newtype IveGotAPlanEffect = IveGotAPlanEffect (Attrs Effect)
  deriving anyclass (IsEffect)
  deriving newtype (Show, Eq, ToJSON, FromJSON, IsRef)

iveGotAPlanEffect :: CardEffect IveGotAPlanEffect
iveGotAPlanEffect =
  effectWith IveGotAPlanEffect Cards.iveGotAPlan $
    (modifiersL .~ [ThwartModifier 1]) . (endsL ?~ DisableAtEndOfPhase)

instance RunMessage IveGotAPlanEffect where
  runMessage msg (IveGotAPlanEffect attrs) = IveGotAPlanEffect <$> runMessage msg attrs
