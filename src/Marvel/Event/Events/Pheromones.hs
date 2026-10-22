module Marvel.Event.Events.Pheromones (
  pheromones,
  Pheromones (..),
) where

import Marvel.Prelude

import Marvel.Card.Code
import Marvel.Damage
import Marvel.Entity
import Marvel.Event.Cards qualified as Cards
import Marvel.Event.Types
import Marvel.Id
import Marvel.Matchers
import Marvel.Message.Lifted
import Marvel.Modifier
import Marvel.Query
import Marvel.Ref

pheromones :: EventCard Pheromones
pheromones = event Pheromones Cards.pheromones

newtype Pheromones = Pheromones (Attrs Event)
  deriving anyclass (IsEvent, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

-- | Stun and confuse an enemy.
instance RunMessage Pheromones where
  runMessage msg e@(Pheromones attrs) = runQueueT $ case msg of
    EventMessage ident (PlayedEvent identityId _ _) | ident == eventId attrs -> do
      enemies <- selectList AnyEnemy
      chooseOne identityId [targeting (enemyRef enemy) (stun enemy attrs >> confuse enemy attrs) | enemy <- enemies]
      pure e
    _ -> Pheromones <$> runMessage msg attrs
