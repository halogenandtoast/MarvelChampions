module Marvel.Event.Events.Schadenfreude (
  schadenfreude,
  Schadenfreude (..),
  schadenfreudeEffect,
  SchadenfreudeEffect (..),
) where

import Marvel.Prelude

import Marvel.Card.Code
import Marvel.Effect.Types
import Marvel.Event.Cards qualified as Cards
import Marvel.Event.Types
import Marvel.Id (CharacterId (..))
import Marvel.Matchers
import Marvel.Message.Lifted
import Marvel.Modifier
import Marvel.Ref
import Marvel.Window qualified as W

schadenfreude :: EventCard Schadenfreude
schadenfreude = event Schadenfreude Cards.schadenfreude

newtype Schadenfreude = Schadenfreude (Attrs Event)
  deriving anyclass (IsEvent, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

-- | Hero Action: Until the end of the turn, heal 2 damage from Rocket Raccoon each time
-- you deal any amount of damage to an enemy.
instance RunMessage Schadenfreude where
  runMessage msg e@(Schadenfreude attrs) = runQueueT $ case msg of
    EventMessage ident (PlayedEvent you _ _) | ident == eventId attrs -> do
      createEffect Cards.schadenfreude attrs (IdentityEntity $ IdentityWithId you)
      pure e
    _ -> Schadenfreude <$> runMessage msg attrs

newtype SchadenfreudeEffect = SchadenfreudeEffect (Attrs Effect)
  deriving anyclass (IsEffect)
  deriving newtype (Show, Eq, ToJSON, FromJSON, IsRef)

schadenfreudeEffect :: CardEffect SchadenfreudeEffect
schadenfreudeEffect =
  effectWith SchadenfreudeEffect Cards.schadenfreude (endsL ?~ DisableAtEndOfTurn)

-- | Each enemy damaged is a time (ruling): one heal per 'W.DamagedEnemy' window, which
-- opens only when some damage got past tough. "You" is your identity or a card you
-- control, not an ally ('DamageByPlayer').
instance RunMessage SchadenfreudeEffect where
  runMessage msg e@(SchadenfreudeEffect attrs) = runQueueT $ case msg of
    CheckWindows ws | IdentityEntity (IdentityWithId you) <- effectMatcher attrs -> do
      for_ ws \case
        W.Window W.After (W.DamagedEnemy _ source damage) ->
          whenM (damageMatchesFrom (DamageByPlayer $ IdentityWithId you) damage source) $
            heal (IdentityCharacter you) 2
        _ -> pure ()
      pure e
    _ -> SchadenfreudeEffect <$> runMessage msg attrs
