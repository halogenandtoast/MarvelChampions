module Marvel.Event.Events.YooHoo where

import Marvel.Prelude
import Marvel.Card.Code
import Marvel.Damage ()
import Marvel.Entity
import Marvel.Event.Cards qualified as Cards
import Marvel.Event.Types
import Marvel.Matchers
import Marvel.Message.Lifted
import Marvel.Modifier
import Marvel.Payment
import Marvel.Ref

yooHoo :: EventCard YooHoo
yooHoo = event YooHoo Cards.yooHoo

newtype YooHoo = YooHoo (Attrs Event)
  deriving anyclass (IsEvent, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

instance RunMessage YooHoo where
  runMessage msg e@(YooHoo attrs) = runQueueT $ case msg of
    EventMessage eid (PlayedEvent you payment _) | eid == eventId attrs -> do
      let amount = sum [n | DamagePayment n <- payments payment]
      removeThreat you attrs amount ThwartableScheme
      pure e
    _ -> YooHoo <$> runMessage msg attrs
