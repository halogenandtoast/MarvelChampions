module Marvel.Event.Events.HomelandIntervention where

import Marvel.Prelude
import Marvel.Ability
import Marvel.Card.Code
import Marvel.Entity
import Marvel.Message.Lifted
import Marvel.Modifier
import Marvel.Ref
import Marvel.Event.Cards qualified as Cards
import Marvel.Event.Types
import Marvel.Matchers
import Marvel.Payment

homelandIntervention :: EventCard HomelandIntervention
homelandIntervention = event HomelandIntervention Cards.homelandIntervention

newtype HomelandIntervention = HomelandIntervention (Attrs Event)
  deriving anyclass (IsEvent, HasAbilities, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

instance RunMessage HomelandIntervention where
  runMessage msg e@(HomelandIntervention attrs) = runQueueT $ case msg of
    EventMessage eid (PlayedEvent you payment _) | eid == eventId attrs -> do
      let n = 2 * fromIntegral (length $ paymentExhaustedCards payment)
      removeThreat you attrs n ThwartableScheme
      pure e
    _ -> HomelandIntervention <$> runMessage msg attrs
