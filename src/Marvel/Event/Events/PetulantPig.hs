module Marvel.Event.Events.PetulantPig where

import Marvel.Prelude
import Marvel.Query
import Marvel.Ability
import Marvel.Card.Code
import Marvel.Entity
import Marvel.Message.Lifted
import Marvel.Modifier
import Marvel.Ref
import Marvel.Event.Cards qualified as Cards
import Marvel.Event.Types

petulantPig :: EventCard PetulantPig
petulantPig = event PetulantPig Cards.petulantPig

newtype PetulantPig = PetulantPig (Attrs Event)
  deriving anyclass (IsEvent, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

instance RunMessage PetulantPig where
  runMessage msg e@(PetulantPig attrs) = runQueueT $ case msg of
    EventMessage eid (PlayedEvent you _ _) | eid == eventId attrs -> do
      withTheVillain you $ \vid -> enemyAttacks vid you
      drawCards you 3
      pure e
    _ -> PetulantPig <$> runMessage msg attrs
