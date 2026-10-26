module Marvel.Event.Events.PinpointStrike (
  pinpointStrike,
  PinpointStrike (..),
) where

import Marvel.Prelude

import Marvel.Card.Code
import Marvel.Damage
import Marvel.Entity
import Marvel.Event.Cards qualified as Cards
import Marvel.Event.Types
import Marvel.Matchers
import Marvel.Message.Lifted
import Marvel.Modifier
import Marvel.Ref
import Marvel.Trait (Trait (Tiny))

pinpointStrike :: EventCard PinpointStrike
pinpointStrike = event PinpointStrike Cards.pinpointStrike

newtype PinpointStrike = PinpointStrike (Attrs Event)
  deriving anyclass (IsEvent, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

-- | Deal 7 damage to an enemy; in Tiny form 1 more, and the attack gains overkill.
instance RunMessage PinpointStrike where
  runMessage msg e@(PinpointStrike attrs) = runQueueT $ case msg of
    EventMessage ident (PlayedEvent you _ _) | ident == eventId attrs -> do
      tiny <- identityMatches (IdentityWithTrait Tiny) you
      let damage = toDamage 7 (FromPlayerAttack you)
      chooseDamage you attrs (if tiny then withOverkill damage {damageAmount = 8} else damage) AttackableEnemy
      pure e
    _ -> PinpointStrike <$> runMessage msg attrs
