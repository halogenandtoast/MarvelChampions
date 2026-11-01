module Marvel.Event.Events.HamItUp where

import Marvel.Prelude
import Marvel.Identity.Types (Field (PlayerIdentityCounters))
import Marvel.Projection
import Marvel.Matchers
import Marvel.Ability ()
import Marvel.Card.Code
import Marvel.Entity
import Marvel.Message.Lifted
import Marvel.Modifier
import Marvel.Ref
import Marvel.Event.Cards qualified as Cards
import Marvel.Event.Types

hamItUp :: EventCard HamItUp
hamItUp = event HamItUp Cards.hamItUp

newtype HamItUp = HamItUp (Attrs Event)
  deriving anyclass (IsEvent, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

instance RunMessage HamItUp where
  runMessage msg e@(HamItUp attrs) = runQueueT $ case msg of
    EventMessage eid (PlayedEvent you _ _) | eid == eventId attrs -> do
      n <- project PlayerIdentityCounters you
      removeThreat you attrs n ThwartableScheme
      pure e
    _ -> HamItUp <$> runMessage msg attrs
