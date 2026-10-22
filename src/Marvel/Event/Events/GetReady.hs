module Marvel.Event.Events.GetReady (
  getReady,
  GetReady (..),
) where

import Marvel.Prelude

import Marvel.Card.Code
import Marvel.Entity
import Marvel.Event.Cards qualified as Cards
import Marvel.Event.Types
import Marvel.Matchers
import Marvel.Message.Lifted hiding (ExhaustedAlly)
import Marvel.Modifier
import Marvel.Query
import Marvel.Ref

getReady :: EventCard GetReady
getReady = event GetReady Cards.getReady

newtype GetReady = GetReady (Attrs Event)
  deriving anyclass (IsEvent, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

instance RunMessage GetReady where
  runMessage msg e@(GetReady attrs) = runQueueT $ case msg of
    EventMessage ident msg' | ident == eventId attrs -> case msg' of
      PlayedEvent identityId _ _ -> do
        allies <- selectList ExhaustedAlly
        chooseOne identityId [targeting ally $ ready ally | ally <- allies]
        pure e
      _ -> GetReady <$> runMessage msg attrs
    _ -> GetReady <$> runMessage msg attrs
