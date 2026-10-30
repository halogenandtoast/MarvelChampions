module Marvel.Event.Events.LightspeedFlight where

import Marvel.Prelude
import Marvel.Card.Code
import Marvel.Entity
import Marvel.Event.Cards qualified as Cards
import Marvel.Event.Types
import Marvel.Matchers
import Marvel.Message.Lifted
import Marvel.Modifier
import Marvel.Ref

lightspeedFlight :: EventCard LightspeedFlight
lightspeedFlight = event LightspeedFlight Cards.lightspeedFlight

newtype LightspeedFlight = LightspeedFlight (Attrs Event)
  deriving anyclass (IsEvent, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

instance RunMessage LightspeedFlight where
  runMessage msg e@(LightspeedFlight attrs) = runQueueT $ case msg of
    EventMessage ident (PlayedEvent you _ _) | ident == eventId attrs ->
      e <$ removeThreat you attrs 3 ThwartableScheme
    _ -> LightspeedFlight <$> runMessage msg attrs
