module Marvel.Event.Events.CreoleCharmer where

import Marvel.Prelude
import Marvel.Card.Code
import Marvel.Count
import Marvel.Entity
import Marvel.Event.Cards qualified as Cards
import Marvel.Event.Types
import Marvel.Matchers
import Marvel.Message.Lifted
import Marvel.Modifier
import Marvel.Query
import Marvel.Ref

creoleCharmer :: EventCard CreoleCharmer
creoleCharmer = event CreoleCharmer Cards.creoleCharmer

newtype CreoleCharmer = CreoleCharmer (Attrs Event)
  deriving anyclass (IsEvent, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

instance RunMessage CreoleCharmer where
  runMessage msg e@(CreoleCharmer attrs) = runQueueT $ case msg of
    EventMessage eid (PlayedEvent you _ _) | eid == eventId attrs -> do
      schemes <- selectList SchemeYouCanThwart
      chooseOne you [targeting (schemeRef scheme) do
        before <- selectCount SchemeThreat (SchemeWithId scheme)
        thwartScheme scheme attrs 3
        cardSpecific attrs "charmerThreat" (you, scheme, before)
        | scheme <- schemes]
      pure e
    CardSpecific (isTarget attrs -> True) "charmerThreat" (fromJSON -> Success (you, scheme, before :: Natural)) -> do
      after <- selectCount SchemeThreat (SchemeWithId scheme)
      when (before > 0 && after == 0) $ withTheVillain you (\vid -> confuse vid attrs)
      pure e
    _ -> CreoleCharmer <$> runMessage msg attrs
