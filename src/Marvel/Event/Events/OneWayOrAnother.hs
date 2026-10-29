module Marvel.Event.Events.OneWayOrAnother (oneWayOrAnother, OneWayOrAnother (..)) where

import Marvel.Prelude

import Marvel.Card.Code
import Marvel.Entity
import Marvel.Event.Cards qualified as Cards
import Marvel.Event.Types
import Marvel.Message.Lifted
import Marvel.Modifier
import Marvel.Ref

oneWayOrAnother :: EventCard OneWayOrAnother
oneWayOrAnother = event OneWayOrAnother Cards.oneWayOrAnother

newtype OneWayOrAnother = OneWayOrAnother (Attrs Event)
  deriving anyclass (IsEvent, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

-- | Revealing the searched side scheme and shuffling are the card's additional
-- cost; by the time its effect runs, every When Revealed has finished.
instance RunMessage OneWayOrAnother where
  runMessage msg a@(OneWayOrAnother attrs) = runQueueT $ case msg of
    EventMessage eid (PlayedEvent ident _ _) | eid == eventId attrs -> a <$ drawCards ident 3
    _ -> OneWayOrAnother <$> runMessage msg attrs
