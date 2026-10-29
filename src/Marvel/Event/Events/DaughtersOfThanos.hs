module Marvel.Event.Events.DaughtersOfThanos (daughtersOfThanos, DaughtersOfThanos (..)) where

import Marvel.Prelude
import Marvel.Card.Code
import Marvel.Entity
import Marvel.Event.Cards qualified as Cards
import Marvel.Event.Types
import Marvel.Message.Lifted
import Marvel.Modifier
import Marvel.Ref

daughtersOfThanos :: EventCard DaughtersOfThanos
daughtersOfThanos = event DaughtersOfThanos Cards.daughtersOfThanos

newtype DaughtersOfThanos = DaughtersOfThanos (Attrs Event)
  deriving anyclass (IsEvent, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

instance RunMessage DaughtersOfThanos where
  runMessage msg a@(DaughtersOfThanos attrs) = runQueueT $ case msg of
    EventMessage eid (PlayedEvent ident _ _) | eid == eventId attrs -> a <$ drawCards ident 3
    _ -> DaughtersOfThanos <$> runMessage msg attrs
