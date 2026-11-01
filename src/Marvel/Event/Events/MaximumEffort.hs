module Marvel.Event.Events.MaximumEffort where

import Marvel.Prelude
import Marvel.Card.Code
import Marvel.Damage
import Marvel.Entity
import Marvel.Event.Cards qualified as Cards
import Marvel.Event.Types
import Marvel.Matchers
import Marvel.Message.Lifted
import Marvel.Modifier
import Marvel.Payment
import Marvel.Ref

maximumEffort :: EventCard MaximumEffort
maximumEffort = event MaximumEffort Cards.maximumEffort

newtype MaximumEffort = MaximumEffort (Attrs Event)
  deriving anyclass (IsEvent, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

instance RunMessage MaximumEffort where
  runMessage msg e@(MaximumEffort attrs) = runQueueT $ case msg of
    EventMessage eid (PlayedEvent you payment _) | eid == eventId attrs -> do
      let amount = sum [n | DamagePayment n <- payments payment]
      chooseDamage you attrs (toDamage amount $ FromPlayerAttack you) (AttackableBy $ IdentityWithId you)
      pure e
    _ -> MaximumEffort <$> runMessage msg attrs
