module Marvel.Event.Events.WindsOfWatoomb (
  windsOfWatoomb,
  WindsOfWatoomb (..),
) where

import Marvel.Prelude

import Marvel.Card.Code
import Marvel.Entity
import Marvel.Event.Cards qualified as Cards
import Marvel.Event.Types
import Marvel.Invocation
import Marvel.Message.Lifted
import Marvel.Modifier
import Marvel.Ref

windsOfWatoomb :: EventCard WindsOfWatoomb
windsOfWatoomb = event WindsOfWatoomb Cards.windsOfWatoomb

newtype WindsOfWatoomb = WindsOfWatoomb (Attrs Event)
  deriving anyclass (IsEvent, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

-- | Special: draw 3 cards.
instance RunMessage WindsOfWatoomb where
  runMessage msg e@(WindsOfWatoomb attrs) = runQueueT $ case msg of
    EventMessage ident msg' | ident == eventId attrs -> case msg' of
      PlayedEvent identityId _ _ -> e <$ drawCards identityId 3
      ResolvedEvent -> e <$ invocationResolved attrs
    _ -> WindsOfWatoomb <$> runMessage msg attrs
