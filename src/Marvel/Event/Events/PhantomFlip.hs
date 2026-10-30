module Marvel.Event.Events.PhantomFlip where

import Marvel.Prelude
import Marvel.Card
import Marvel.Entity
import Marvel.Event.Cards qualified as Cards
import Marvel.Event.Types
import Marvel.Matchers
import Marvel.Message.Lifted
import Marvel.Modifier
import Marvel.Ref

phantomFlip :: EventCard PhantomFlip
phantomFlip = event PhantomFlip Cards.phantomFlip

newtype PhantomFlip = PhantomFlip (Attrs Event)
  deriving anyclass (IsEvent, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

instance RunMessage PhantomFlip where
  runMessage msg e@(PhantomFlip attrs) = runQueueT $ case msg of
    EventMessage eid (PlayedEvent you _ _) | eid == eventId attrs ->
      e <$ removeThreat you attrs 5 ThwartableScheme
    _ -> PhantomFlip <$> runMessage msg attrs
