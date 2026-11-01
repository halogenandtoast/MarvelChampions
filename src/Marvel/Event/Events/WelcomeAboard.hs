module Marvel.Event.Events.WelcomeAboard (
  welcomeAboard,
  welcomeAboardEffect,
  WelcomeAboard (..),
) where

import Marvel.Prelude

import Marvel.Card.Code
import Marvel.Card.Def
import Marvel.Effect.Types
import Marvel.Entity ()
import Marvel.Event.Cards qualified as Cards
import Marvel.Event.Types
import Marvel.Matchers
import Marvel.Message.Lifted
import Marvel.Modifier
import Marvel.Ref

welcomeAboard :: EventCard WelcomeAboard
welcomeAboard = event WelcomeAboard Cards.welcomeAboard

newtype WelcomeAboard = WelcomeAboard (Attrs Event)
  deriving anyclass (IsEvent, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

-- | Reduce the resource cost of the next ally played this phase by 2: by any player
-- (ruling), an effect on every identity.
instance RunMessage WelcomeAboard where
  runMessage msg e@(WelcomeAboard attrs) = runQueueT $ case msg of
    EventMessage ident (PlayedEvent {}) | ident == eventId attrs -> do
      createEffect Cards.welcomeAboard attrs (IdentityEntity AnyIdentity)
      pure e
    _ -> WelcomeAboard <$> runMessage msg attrs

-- | Used up by the first ally any player plays, gone at the end of the phase.
newtype WelcomeAboardEffect = WelcomeAboardEffect (Attrs Effect)
  deriving anyclass (IsEffect)
  deriving newtype (Show, Eq, ToJSON, FromJSON, IsRef)

welcomeAboardEffect :: CardEffect WelcomeAboardEffect
welcomeAboardEffect =
  effectWith WelcomeAboardEffect Cards.welcomeAboard $
    (modifiersL .~ [ResourceCostReductionFor (CardWithType AllyType) 2])
      . (endsL ?~ DisableAtEndOfPhase)

instance RunMessage WelcomeAboardEffect where
  runMessage msg e@(WelcomeAboardEffect attrs) = runQueueT $ case msg of
    IdentityMessage _ (PlayedCard card _) | cardMatch (CardWithType AllyType) card -> do
      disableEffect (effectId attrs)
      pure e
    _ -> WelcomeAboardEffect <$> runMessage msg attrs
