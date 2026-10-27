module Marvel.Event.Events.SetThePace (
  setThePace,
  SetThePace (..),
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

setThePace :: EventCard SetThePace
setThePace = event SetThePace Cards.setThePace

newtype SetThePace = SetThePace (Attrs Event)
  deriving anyclass (IsEvent, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

-- | Remove 1 threat from a scheme.
instance RunMessage SetThePace where
  runMessage msg e@(SetThePace attrs) = runQueueT $ case msg of
    EventMessage ident msg' | ident == eventId attrs -> case msg' of
      PlayedEvent identityId _ _ -> do
        removeThreat identityId attrs 1 ThwartableScheme
        pure e
      _ -> SetThePace <$> runMessage msg attrs
    _ -> SetThePace <$> runMessage msg attrs
