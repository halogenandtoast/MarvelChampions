module Marvel.Event.Events.ReturnTheFavor where

import Marvel.Prelude
import Marvel.Card
import Marvel.Damage
import Marvel.Entity
import Marvel.Event.Cards qualified as Cards
import Marvel.Event.Types
import Marvel.Message.Lifted
import Marvel.Modifier
import Marvel.Ref

returnTheFavor :: EventCard ReturnTheFavor
returnTheFavor = event ReturnTheFavor Cards.returnTheFavor

newtype ReturnTheFavor = ReturnTheFavor (Attrs Event)
  deriving anyclass (IsEvent, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

instance RunMessage ReturnTheFavor where
  runMessage msg e@(ReturnTheFavor attrs) = runQueueT $ case msg of
    EventMessage eid (PlayedEvent you _ _) | eid == eventId attrs -> do
      withTheVillain you $ \villain -> dealDamage villain attrs (toDamage 5 $ FromPlayerAttack you)
      pure e
    _ -> ReturnTheFavor <$> runMessage msg attrs
