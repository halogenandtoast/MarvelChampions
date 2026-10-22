module Marvel.Event.Events.Backflip (
  Backflip,
  backflip,
) where

import Marvel.Prelude

import Marvel.Card.Code
import Marvel.Entity
import Marvel.Event.Cards qualified as Cards
import Marvel.Event.Types
import Marvel.Message.Lifted
import Marvel.Modifier
import Marvel.Ref

backflip :: EventCard Backflip
backflip = event Backflip Cards.backflip

newtype Backflip = Backflip (Attrs Event)
  deriving anyclass (IsEvent, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

instance RunMessage Backflip where
  runMessage msg e@(Backflip attrs) = runQueueT $ case msg of
    EventMessage ident msg' | ident == eventId attrs -> case msg' of
      PlayedEvent identityId _ _ -> do
        cancelDamage identityId
        pure e
      _ -> Backflip <$> runMessage msg attrs
    _ -> Backflip <$> runMessage msg attrs
