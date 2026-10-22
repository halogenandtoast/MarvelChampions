module Marvel.Event.Events.WiggleRoom (
  wiggleRoom,
  WiggleRoom (..),
) where

import Marvel.Prelude

import Marvel.Card.Code
import Marvel.Entity
import Marvel.Event.Cards qualified as Cards
import Marvel.Event.Types
import Marvel.Message
import Marvel.Modifier
import Marvel.Queue
import Marvel.Ref

wiggleRoom :: EventCard WiggleRoom
wiggleRoom = event WiggleRoom Cards.wiggleRoom

newtype WiggleRoom = WiggleRoom (Attrs Event)
  deriving anyclass (IsEvent, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

instance RunMessage WiggleRoom where
  runMessage msg e@(WiggleRoom attrs) = case msg of
    EventMessage ident msg' | ident == eventId attrs -> case msg' of
      PlayedEvent identityId _ _ -> do
        preventDamage identityId 3
        push $ IdentityMessage identityId $ DrawCards FromDeck 1
        pure e
      _ -> WiggleRoom <$> runMessage msg attrs
    _ -> WiggleRoom <$> runMessage msg attrs
