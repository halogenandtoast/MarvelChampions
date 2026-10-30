module Marvel.Event.Events.RapidDeployment where

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

rapidDeployment :: EventCard RapidDeployment
rapidDeployment = event RapidDeployment Cards.rapidDeployment

newtype RapidDeployment = RapidDeployment (Attrs Event)
  deriving anyclass (IsEvent, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

instance RunMessage RapidDeployment where
  runMessage msg e@(RapidDeployment attrs) = runQueueT $ case msg of
    EventMessage ident (PlayedEvent you payment _) | ident == eventId attrs -> do
      removeThreat you attrs 3 ThwartableScheme
      when (syncRatioResourcesPaid payment > 0) $ cardSpecific attrs "again" you
      pure e
    CardSpecific (isTarget attrs -> True) "again" (fromJSON -> Success you) -> do
      removeThreat you attrs 3 ThwartableScheme
      pure e
    _ -> RapidDeployment <$> runMessage msg attrs
