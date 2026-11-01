module Marvel.Event.Events.CommandAuthority where

import Marvel.Prelude
import Marvel.Card
import Marvel.Damage ()
import Marvel.Matchers
import Marvel.Payment
import Marvel.Ability ()
import Marvel.Card.Code ()
import Marvel.Entity
import Marvel.Message.Lifted
import Marvel.Modifier
import Marvel.Ref
import Marvel.Event.Cards qualified as Cards
import Marvel.Event.Types

commandAuthority :: EventCard CommandAuthority
commandAuthority = event CommandAuthority Cards.commandAuthority

newtype CommandAuthority = CommandAuthority (Attrs Event)
  deriving anyclass (IsEvent, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)



instance RunMessage CommandAuthority where
  runMessage msg a@(CommandAuthority attrs) = runQueueT $ case msg of
    EventMessage eid (PlayedEvent you payment _) | eid == eventId attrs -> do
      removeThreat you attrs 3 ThwartableScheme
      when (any (cardMatch $ CardWithType ResourceType) $ paymentSpentCards payment) $ drawCards you 1
      pure a
    _ -> CommandAuthority <$> runMessage msg attrs
