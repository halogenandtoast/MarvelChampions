module Marvel.Event.Events.WeatherGoddess where

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

weatherGoddess :: EventCard WeatherGoddess
weatherGoddess = event WeatherGoddess Cards.weatherGoddess

newtype WeatherGoddess = WeatherGoddess (Attrs Event)
  deriving anyclass (IsEvent, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

instance RunMessage WeatherGoddess where
  runMessage msg e@(WeatherGoddess attrs) = runQueueT $ case msg of
    EventMessage eid (PlayedEvent you _ _) | eid == eventId attrs -> do
      swapWeather you
      pure e
    _ -> WeatherGoddess <$> runMessage msg attrs
