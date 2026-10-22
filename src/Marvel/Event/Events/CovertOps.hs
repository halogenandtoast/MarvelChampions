module Marvel.Event.Events.CovertOps (
  covertOps,
  CovertOps (..),
) where

import Marvel.Prelude

import Marvel.Card.Code
import Marvel.Entity
import Marvel.Event.Cards qualified as Cards
import Marvel.Event.Types
import Marvel.Matchers
import Marvel.Message.Lifted
import Marvel.Modifier
import Marvel.Query
import Marvel.Ref

covertOps :: EventCard CovertOps
covertOps = event CovertOps Cards.covertOps

newtype CovertOps = CovertOps (Attrs Event)
  deriving anyclass (IsEvent, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

instance RunMessage CovertOps where
  runMessage msg e@(CovertOps attrs) = runQueueT $ case msg of
    EventMessage ident (PlayedEvent identityId _ _) | ident == eventId attrs -> do
      -- with no villain in play (as between one Kang and the next), nothing
      selectOne ActiveVillain >>= \case
        Nothing -> pure e
        Just villain -> do
          removeThreat identityId attrs 4 ThwartableScheme
          confuse villain attrs
          pure e
    _ -> CovertOps <$> runMessage msg attrs
