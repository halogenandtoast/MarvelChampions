module Marvel.Event.Events.CovertOps (
  covertOps,
  CovertOps (..),
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
import Marvel.Query
import Marvel.Question
import Marvel.Queue
import Marvel.Ref

covertOps :: EventCard CovertOps
covertOps = event CovertOps Cards.covertOps

newtype CovertOps = CovertOps (Attrs Event)
  deriving anyclass (IsEvent, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

instance RunMessage CovertOps where
  runMessage msg e@(CovertOps attrs) = case msg of
    EventMessage ident (PlayedEvent identityId _ _) | ident == eventId attrs -> do
      villain <- selectJust ActiveVillain
      thwart <- choiceMessages identityId (RemoveThreat (toSource attrs) 4 ThwartableScheme)
      pushAll $ thwart <> [VillainMessage villain $ VillainConfused (toSource attrs)]
      pure e
    _ -> CovertOps <$> runMessage msg attrs
