module Marvel.Event.Events.TorrentialRain where

import Marvel.Prelude
import Marvel.Ability
import Marvel.Card
import Marvel.Damage
import Marvel.Entity
import Marvel.Event.Cards qualified as Cards
import Marvel.Event.Types
import Marvel.Id
import Marvel.Matchers
import Marvel.Message.Lifted
import Marvel.Modifier
import Marvel.Query
import Marvel.Ref

torrentialRain :: EventCard TorrentialRain
torrentialRain = event TorrentialRain Cards.torrentialRain

newtype TorrentialRain = TorrentialRain (Attrs Event)
  deriving anyclass (IsEvent, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

instance RunMessage TorrentialRain where
  runMessage msg e@(TorrentialRain attrs) = runQueueT $ case msg of
    EventMessage eid (PlayedEvent you _ _) | eid == eventId attrs -> do
      thwartDivided you attrs 3 ThwartableScheme
      cardSpecific attrs "weather" you
      pure e
    CardSpecific (isTarget attrs -> True) "weather" (fromJSON -> Success (you :: IdentityId)) -> do
      weather <- selectList (SupportNamed "Hurricane")
      for_ weather (resolveWeatherSpecial you)
      pure e
    _ -> TorrentialRain <$> runMessage msg attrs
