module Marvel.Event.Events.UnstoppableForce (
  unstoppableForce,
  UnstoppableForce (..),
) where

import Marvel.Prelude

import Marvel.Card.Code
import Marvel.Entity
import Marvel.Event.Cards qualified as Cards
import Marvel.Event.Types
import Marvel.Message.Lifted
import Marvel.Modifier
import Marvel.Payment
import Marvel.Ref
import Marvel.Resource

unstoppableForce :: EventCard UnstoppableForce
unstoppableForce = event UnstoppableForce Cards.unstoppableForce

newtype UnstoppableForce = UnstoppableForce (Attrs Event)
  deriving anyclass (IsEvent, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

instance RunMessage UnstoppableForce where
  runMessage msg e@(UnstoppableForce attrs) = runQueueT $ case msg of
    EventMessage ident msg' | ident == eventId attrs -> case msg' of
      PlayedEvent identityId payment _ -> do
        physical <- paidUsingOnly Physical payment
        ready identityId
        when physical $ drawCards identityId 1
        pure e
      _ -> UnstoppableForce <$> runMessage msg attrs
    _ -> UnstoppableForce <$> runMessage msg attrs
