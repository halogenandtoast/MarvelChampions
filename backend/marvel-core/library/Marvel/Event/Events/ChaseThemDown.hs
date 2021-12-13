module Marvel.Event.Events.ChaseThemDown
  ( chaseThemDown
  , ChaseThemDown(..)
  )
where

import Marvel.Prelude

import Marvel.Card.Code
import Marvel.Entity
import Marvel.Event.Attrs
import Marvel.Event.Cards qualified as Cards
import Marvel.Matchers
import Marvel.Message
import Marvel.Modifier
import Marvel.Question
import Marvel.Source
import Marvel.Target

chaseThemDown :: EventCard ChaseThemDown
chaseThemDown = event ChaseThemDown Cards.chaseThemDown

newtype ChaseThemDown = ChaseThemDown EventAttrs
  deriving anyclass (IsEvent, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, Entity, IsSource, IsTarget)

instance RunMessage ChaseThemDown where
  runMessage msg e@(ChaseThemDown attrs) = case msg of
    EventMessage eid msg' | eid == toId e -> case msg' of
      PlayedEvent identityId _ _ ->
        e <$ pushChoice identityId (RemoveThreat (toSource attrs) 2 ThwartableScheme)
      _ -> ChaseThemDown <$> runMessage msg attrs
    _ -> ChaseThemDown <$> runMessage msg attrs
