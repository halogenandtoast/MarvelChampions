module Marvel.Event.Events.SureGamble (
  sureGamble,
  sureGambleEffect,
  SureGamble (..),
  SureGambleEffect (..),
) where

import Marvel.Prelude

import Marvel.Card.Code
import Marvel.Effect.Types
import Marvel.Entity ()
import Marvel.Event.Cards qualified as Cards
import Marvel.Event.Types
import Marvel.Matchers
import Marvel.Message.Lifted
import Marvel.Modifier
import Marvel.Ref

sureGamble :: EventCard SureGamble
sureGamble = event SureGamble Cards.sureGamble

newtype SureGamble = SureGamble (Attrs Event)
  deriving anyclass (IsEvent, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

-- | The next card played this phase costs 3 less: by any player, as the text names none.
-- Sure Gamble's own play is already counted, so the next is another card. A future
-- target, so it needs none to be played (RR Target).
instance RunMessage SureGamble where
  runMessage msg e@(SureGamble attrs) = runQueueT $ case msg of
    EventMessage ident (PlayedEvent _ _ _) | ident == eventId attrs -> do
      createEffect Cards.sureGamble attrs (IdentityEntity AnyIdentity)
      pure e
    _ -> SureGamble <$> runMessage msg attrs

-- | Gone once a card is played, or at the end of the phase.
newtype SureGambleEffect = SureGambleEffect (Attrs Effect)
  deriving anyclass (IsEffect)
  deriving newtype (Show, Eq, ToJSON, FromJSON, IsRef)

sureGambleEffect :: CardEffect SureGambleEffect
sureGambleEffect =
  effectWith SureGambleEffect Cards.sureGamble $
    (modifiersL .~ [ResourceCostReduction 3]) . (endsL ?~ DisableAtEndOfPhase)

instance RunMessage SureGambleEffect where
  runMessage msg e@(SureGambleEffect attrs) = runQueueT $ case msg of
    IdentityMessage _ (PlayedCard _ _) -> do
      disableEffect (effectId attrs)
      pure e
    _ -> SureGambleEffect <$> runMessage msg attrs
