module Marvel.Event.Events.YawAndRoll where

import Marvel.Prelude
import Marvel.Card.Code
import Marvel.Entity
import Marvel.Event.Cards qualified as Cards
import Marvel.Event.Types
import Marvel.Matchers
import Marvel.Message.Lifted
import Marvel.Modifier
import Marvel.Ref

yawAndRoll :: EventCard YawAndRoll
yawAndRoll = event YawAndRoll Cards.yawAndRoll

newtype YawAndRoll = YawAndRoll (Attrs Event)
  deriving anyclass (IsEvent, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

instance RunMessage YawAndRoll where
  runMessage msg e@(YawAndRoll attrs) = runQueueT $ case msg of
    EventMessage ident (PlayedEvent you _ _) | ident == eventId attrs -> do
      removeThreat you attrs 3 ThwartableScheme
      pure e
    _ -> YawAndRoll <$> runMessage msg attrs
