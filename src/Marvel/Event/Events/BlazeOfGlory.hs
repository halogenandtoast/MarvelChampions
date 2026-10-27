module Marvel.Event.Events.BlazeOfGlory (
  blazeOfGlory,
  blazeOfGloryEffect,
  BlazeOfGloryEffect (..),
  BlazeOfGlory (..),
) where

import Marvel.Prelude

import Marvel.Card.Code
import Marvel.Damage
import Marvel.Effect.Types
import Marvel.Event.Cards qualified as Cards
import Marvel.Event.Types
import Marvel.Matchers
import Marvel.Message.Lifted
import Marvel.Modifier
import Marvel.Ref
import Marvel.Trait

blazeOfGlory :: EventCard BlazeOfGlory
blazeOfGlory = event BlazeOfGlory Cards.blazeOfGlory

newtype BlazeOfGlory = BlazeOfGlory (Attrs Event)
  deriving anyclass (IsEvent, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

-- | "Each Guardian character gets +2 THW and +2 ATK this phase": a lasting effect, so
-- a character that is a Guardian later in the phase gets it too (RR "Lasting Effects").
instance RunMessage BlazeOfGlory where
  runMessage msg e@(BlazeOfGlory attrs) = runQueueT $ case msg of
    EventMessage ident msg' | ident == eventId attrs -> case msg' of
      PlayedEvent {} -> do
        for_ [IdentityEntity $ IdentityWithTrait Guardian, AllyEntity $ AllyWithTrait Guardian] $
          createEffect Cards.blazeOfGlory e
        pure e
      _ -> BlazeOfGlory <$> runMessage msg attrs
    _ -> BlazeOfGlory <$> runMessage msg attrs

newtype BlazeOfGloryEffect = BlazeOfGloryEffect (Attrs Effect)
  deriving anyclass (IsEffect)
  deriving newtype (Show, Eq, ToJSON, FromJSON, IsRef)

blazeOfGloryEffect :: CardEffect BlazeOfGloryEffect
blazeOfGloryEffect =
  effectWith
    BlazeOfGloryEffect
    Cards.blazeOfGlory
    (modifiersL .~ [ThwartModifier 2, AttackModifier 2])

-- | "At the end of the phase, deal 1 damage to each Guardian character": each in play
-- then, any player's (ruling). The identities' effect deals it, once for both.
instance RunMessage BlazeOfGloryEffect where
  runMessage msg e@(BlazeOfGloryEffect attrs) = runQueueT $ case msg of
    EndPhase _ -> do
      case effectMatcher attrs of
        IdentityEntity _ ->
          damageAllCharacters (effectSource attrs) (toDamage 1 FromAbility) $
            CharacterControlledBy AnyIdentity <> CharacterWithTrait Guardian
        _ -> pure ()
      e <$ disableEffect (effectId attrs)
    _ -> BlazeOfGloryEffect <$> runMessage msg attrs
