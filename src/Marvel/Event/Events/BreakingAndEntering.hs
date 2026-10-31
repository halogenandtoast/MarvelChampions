module Marvel.Event.Events.BreakingAndEntering where

import Marvel.Prelude
import Marvel.Card.Code
import Marvel.Entity
import Marvel.Event.Cards qualified as Cards
import Marvel.Event.Types
import Marvel.Matchers
import Marvel.Message.Lifted
import Marvel.Modifier
import Marvel.Ref

breakingAndEntering :: EventCard BreakingAndEntering
breakingAndEntering = event BreakingAndEntering Cards.breakingAndEntering

newtype BreakingAndEntering = BreakingAndEntering (Attrs Event)
  deriving anyclass (IsEvent, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

instance RunMessage BreakingAndEntering where
  runMessage msg a@(BreakingAndEntering attrs) = runQueueT $ case msg of
    EventMessage eid (PlayedEvent you _ _) | eid == eventId attrs -> a <$ removeThreat you attrs 3 SchemeYouCanThwart
    _ -> BreakingAndEntering <$> runMessage msg attrs
