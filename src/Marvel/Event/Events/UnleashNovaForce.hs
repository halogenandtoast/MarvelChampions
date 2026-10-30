module Marvel.Event.Events.UnleashNovaForce where

import Marvel.Prelude
import Marvel.Card.Code
import Marvel.Effect.Types
import Marvel.Event.Cards qualified as Cards
import Marvel.Event.Types
import Marvel.Matchers
import Marvel.Message.Lifted
import Marvel.Modifier
import Marvel.Ref
import Marvel.Window qualified as W

unleashNovaForce :: EventCard UnleashNovaForce
unleashNovaForce = event UnleashNovaForce Cards.unleashNovaForce

newtype UnleashNovaForce = UnleashNovaForce (Attrs Event)
  deriving anyclass (IsEvent, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

instance RunMessage UnleashNovaForce where
  runMessage msg e@(UnleashNovaForce attrs) = runQueueT $ case msg of
    EventMessage ident (PlayedEvent you _ _) | ident == eventId attrs ->
      e <$ createEffect Cards.unleashNovaForce attrs (IdentityEntity $ IdentityWithId you)
    _ -> UnleashNovaForce <$> runMessage msg attrs

newtype UnleashNovaForceEffect = UnleashNovaForceEffect (Attrs Effect)
  deriving anyclass (IsEffect)
  deriving newtype (Show, Eq, ToJSON, FromJSON, IsRef)

unleashNovaForceEffect :: CardEffect UnleashNovaForceEffect
unleashNovaForceEffect = effectWith UnleashNovaForceEffect Cards.unleashNovaForce (endsL ?~ DisableAtEndOfRound)

instance RunMessage UnleashNovaForceEffect where
  runMessage msg e@(UnleashNovaForceEffect attrs) = runQueueT $ case msg of
    CheckWindows windows | IdentityEntity (IdentityWithId you) <- effectMatcher attrs -> do
      let matcher = W.WindowOneOf
            [ W.EnemyDefeated W.After AnyEnemy (DamageByHero $ IdentityWithId you <> HeroIdentity)
            , W.RemovesLastThreat (IdentityWithId you <> HeroIdentity)
            ]
      for_ windows \window -> whenM (W.windowMatches matcher window (toSource attrs)) do
        ready you
        drawCards you 1
      pure e
    _ -> UnleashNovaForceEffect <$> runMessage msg attrs
