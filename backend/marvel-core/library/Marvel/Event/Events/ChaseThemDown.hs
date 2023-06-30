module Marvel.Event.Events.ChaseThemDown (
  chaseThemDown,
  ChaseThemDown (..),
) where

import Marvel.Prelude

import Marvel.Card.Code
import Marvel.Choice
import Marvel.Entity
import Marvel.Event.Cards qualified as Cards
import Marvel.Event.Types
import Marvel.Matchers
import Marvel.Message
import Marvel.Modifier
import Marvel.Question
import Marvel.Ref

chaseThemDown :: EventCard ChaseThemDown
chaseThemDown = event ChaseThemDown Cards.chaseThemDown

newtype ChaseThemDown = ChaseThemDown (Attrs Event)
  deriving anyclass (IsEvent, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

instance RunMessage ChaseThemDown where
  runMessage msg e@(ChaseThemDown attrs) = case msg of
    EventMessage ident msg' | ident == eventId attrs -> case msg' of
      PlayedEvent identityId _ _ ->
        e
          <$ pushChoice
            identityId
            (RemoveThreat (toSource attrs) 2 ThwartableScheme)
      _ -> ChaseThemDown <$> runMessage msg attrs
    _ -> ChaseThemDown <$> runMessage msg attrs
