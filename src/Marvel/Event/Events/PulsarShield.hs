module Marvel.Event.Events.PulsarShield (
  pulsarShield,
  pulsarShieldEffect,
  PulsarShield (..),
  PulsarShieldEffect (..),
) where

import Marvel.Prelude

import Marvel.Card.Code
import Marvel.Effect.Types
import Marvel.Entity ()
import Marvel.Event.Cards qualified as Cards
import Marvel.Event.Types
import Marvel.Keyword
import Marvel.Matchers
import Marvel.Message.Lifted
import Marvel.Modifier
import Marvel.Ref
import Marvel.Upgrade.Cards qualified as Upgrades

pulsarShield :: EventCard PulsarShield
pulsarShield = event PulsarShield Cards.pulsarShield

newtype PulsarShield = PulsarShield (Attrs Event)
  deriving anyclass (IsEvent, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

-- | When Spectrum defends, change to Pulsar energy form and ready Spectrum. If you were
-- already in Pulsar energy form, she gains retaliate 1 until the end of the phase.
instance RunMessage PulsarShield where
  runMessage msg e@(PulsarShield attrs) = runQueueT $ case msg of
    EventMessage ident (PlayedEvent you _ _) | ident == eventId attrs -> do
      already <- isInEnergyForm you Upgrades.pulsar
      changeToEnergyForm you Upgrades.pulsar
      ready you
      when already $ createEffect Cards.pulsarShield e (IdentityEntity $ IdentityWithId you)
      pure e
    _ -> PulsarShield <$> runMessage msg attrs

newtype PulsarShieldEffect = PulsarShieldEffect (Attrs Effect)
  deriving anyclass (IsEffect)
  deriving newtype (Show, Eq, ToJSON, FromJSON, IsRef)

pulsarShieldEffect :: CardEffect PulsarShieldEffect
pulsarShieldEffect =
  effectWith PulsarShieldEffect Cards.pulsarShield $
    (modifiersL .~ [KeywordModifier (Retaliate 1)]) . (endsL ?~ DisableAtEndOfPhase)

instance RunMessage PulsarShieldEffect where
  runMessage msg (PulsarShieldEffect attrs) = PulsarShieldEffect <$> runMessage msg attrs
