module Marvel.Event.Events.SideStep (
  sideStep,
  SideStep (..),
) where

import Marvel.Prelude

import Marvel.Attack (sourceEnemy)
import Marvel.Card.Code
import Marvel.Damage
import Marvel.Entity
import Marvel.Event.Cards qualified as Cards
import Marvel.Event.Types
import Marvel.Message.Lifted
import Marvel.Modifier
import Marvel.Payment
import Marvel.Ref
import Marvel.Resource
import Marvel.Window qualified as W

sideStep :: EventCard SideStep
sideStep = event SideStep Cards.sideStep

newtype SideStep = SideStep (Attrs Event)
  deriving anyclass (IsEvent, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

-- | When you would take damage, prevent 3 of it; paid for with an energy resource (a
-- wild only declared as energy), deal 1 damage to the enemy the damage is from, if it
-- is from one.
instance RunMessage SideStep where
  runMessage msg e@(SideStep attrs) = runQueueT $ case msg of
    EventMessage ident (PlayedEvent _ payment (Just (W.IdentityTakeDamage hero _))) | ident == eventId attrs -> do
      -- read before the damage is prevented, which may take it off the queue
      enemy <- (sourceEnemy =<<) <$> pendingDamageSource hero
      energy <- paidWith Energy payment
      preventDamage hero 3
      when energy $ for_ enemy \en -> dealDamage en attrs (toDamage 1 FromAbility)
      pure e
    _ -> SideStep <$> runMessage msg attrs
