module Marvel.Event.Events.GetReady
  ( getReady
  , GetReady(..)
  ) where

import Marvel.Prelude

import Marvel.Card.Code
import Marvel.Choice
import Marvel.Entity
import Marvel.Event.Cards qualified as Cards
import Marvel.Event.Types
import Marvel.Matchers
import Marvel.Message hiding (ExhaustedAlly)
import Marvel.Modifier
import Marvel.Query
import Marvel.Question
import Marvel.Source
import Marvel.Target

getReady :: EventCard GetReady
getReady = event GetReady Cards.getReady

newtype GetReady = GetReady (Attrs Event)
  deriving anyclass (IsEvent, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsSource, IsTarget)

instance RunMessage GetReady where
  runMessage msg e@(GetReady attrs) = case msg of
    EventMessage ident msg' | ident == eventId attrs -> case msg' of
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
