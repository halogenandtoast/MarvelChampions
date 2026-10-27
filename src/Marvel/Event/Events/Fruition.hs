module Marvel.Event.Events.Fruition (
  fruition,
  Fruition (..),
) where

import Marvel.Prelude

import Marvel.Card.Code
import Marvel.Entity
import Marvel.Event.Cards qualified as Cards
import Marvel.Event.Types
import Marvel.Message.Lifted
import Marvel.Modifier
import Marvel.Ref

fruition :: EventCard Fruition
fruition = event Fruition Cards.fruition

newtype Fruition = Fruition (Attrs Event)
  deriving anyclass (IsEvent, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

-- | Action: place 2 growth counters on Groot (to a maximum of 10).
instance RunMessage Fruition where
  runMessage msg e@(Fruition attrs) = runQueueT $ case msg of
    EventMessage ident (PlayedEvent you _ _) | ident == eventId attrs -> do
      placeGrowthCounters you 2
      pure e
    _ -> Fruition <$> runMessage msg attrs
