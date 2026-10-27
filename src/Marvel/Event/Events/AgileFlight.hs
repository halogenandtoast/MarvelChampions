module Marvel.Event.Events.AgileFlight (
  agileFlight,
  AgileFlight (..),
) where

import Marvel.Prelude

import Marvel.Card.Code
import Marvel.Entity
import Marvel.Event.Cards qualified as Cards
import Marvel.Event.Types
import Marvel.Matchers
import Marvel.Message.Lifted
import Marvel.Modifier
import Marvel.Ref

agileFlight :: EventCard AgileFlight
agileFlight = event AgileFlight Cards.agileFlight

newtype AgileFlight = AgileFlight (Attrs Event)
  deriving anyclass (IsEvent, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

-- | Remove a total of up to 5 threat from among schemes (as you choose).
instance RunMessage AgileFlight where
  runMessage msg e@(AgileFlight attrs) = runQueueT $ case msg of
    EventMessage ident (PlayedEvent you _ _) | ident == eventId attrs -> do
      thwartDividedUpTo you attrs 5 ThwartableScheme
      pure e
    _ -> AgileFlight <$> runMessage msg attrs
