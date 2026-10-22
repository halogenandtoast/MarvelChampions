module Marvel.Event.Events.ToTheRescue (
  toTheRescue,
  ToTheRescue (..),
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

toTheRescue :: EventCard ToTheRescue
toTheRescue = event ToTheRescue Cards.toTheRescue

newtype ToTheRescue = ToTheRescue (Attrs Event)
  deriving anyclass (IsEvent, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

instance RunMessage ToTheRescue where
  runMessage msg e@(ToTheRescue attrs) = runQueueT $ case msg of
    EventMessage ident msg' | ident == eventId attrs -> case msg' of
      PlayedEvent identityId _ _ -> do
        removeThreat identityId attrs 2 ThwartableScheme
        pure e
      _ -> ToTheRescue <$> runMessage msg attrs
    _ -> ToTheRescue <$> runMessage msg attrs
