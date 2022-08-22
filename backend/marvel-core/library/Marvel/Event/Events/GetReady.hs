module Marvel.Event.Events.GetReady
  ( getReady
  , GetReady(..)
  ) where

import Marvel.Prelude

import Marvel.Card.Code
import Marvel.Entity
import Marvel.Event.Types
import Marvel.Event.Cards qualified as Cards
import Marvel.Matchers
import Marvel.Message hiding (ExhaustedAlly)
import Marvel.Modifier
import Marvel.Query
import Marvel.Question
import Marvel.Source
import Marvel.Target

getReady :: EventCard GetReady
getReady = event GetReady Cards.getReady

newtype GetReady = GetReady EventAttrs
  deriving anyclass (IsEvent, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, Entity, IsSource, IsTarget)

instance RunMessage GetReady where
  runMessage msg e@(GetReady attrs) = case msg of
    EventMessage eid msg' | eid == toId e -> case msg' of
      PlayedEvent identityId _ _ -> do
        allies <- selectList ExhaustedAlly
        chooseOne identityId $ map
          (\aid ->
            TargetLabel (AllyTarget aid) [Run [AllyMessage aid ReadiedAlly]]
          )
          allies
        pure e
      _ -> GetReady <$> runMessage msg attrs
    _ -> GetReady <$> runMessage msg attrs
