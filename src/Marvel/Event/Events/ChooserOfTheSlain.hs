module Marvel.Event.Events.ChooserOfTheSlain (chooserOfTheSlain, ChooserOfTheSlain (..)) where

import Marvel.Prelude
import Marvel.Card
import Marvel.Entity
import Marvel.Event.Cards qualified as Cards
import Marvel.Event.Types
import Marvel.Matchers ()
import Marvel.Message.Lifted
import Marvel.Modifier
import Marvel.Query ()
import Marvel.Ref

chooserOfTheSlain :: EventCard ChooserOfTheSlain
chooserOfTheSlain = event ChooserOfTheSlain Cards.chooserOfTheSlain

newtype ChooserOfTheSlain = ChooserOfTheSlain (Attrs Event)
  deriving anyclass (IsEvent, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

-- Searching and putting the minion into play is the card's additional cost, so
-- its engagement and quickstrike finish before this event starts drawing cards.
instance RunMessage ChooserOfTheSlain where
  runMessage msg e@(ChooserOfTheSlain attrs) = runQueueT $ case msg of
    EventMessage eid (PlayedEvent you _ _) | eid == eventId attrs -> e <$ drawCards you 2
    _ -> ChooserOfTheSlain <$> runMessage msg attrs
