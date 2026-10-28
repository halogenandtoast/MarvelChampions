module Marvel.Event.Events.SpeedOfLight (
  speedOfLight,
  SpeedOfLight (..),
) where

import Marvel.Prelude

import Marvel.Card.Code
import Marvel.Entity
import Marvel.Event.Cards qualified as Cards
import Marvel.Event.Types
import Marvel.Message.Lifted
import Marvel.Modifier
import Marvel.Ref

speedOfLight :: EventCard SpeedOfLight
speedOfLight = event SpeedOfLight Cards.speedOfLight

newtype SpeedOfLight = SpeedOfLight (Attrs Event)
  deriving anyclass (IsEvent, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

-- | Change energy forms and draw 1 card. Under Loss of Control it only draws (R26).
instance RunMessage SpeedOfLight where
  runMessage msg e@(SpeedOfLight attrs) = runQueueT $ case msg of
    EventMessage ident (PlayedEvent you _ _) | ident == eventId attrs -> do
      changeEnergyForm you
      drawCards you 1
      pure e
    _ -> SpeedOfLight <$> runMessage msg attrs
