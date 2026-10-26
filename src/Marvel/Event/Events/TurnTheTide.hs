module Marvel.Event.Events.TurnTheTide (
  turnTheTide,
  TurnTheTide (..),
) where

import Marvel.Prelude

import Marvel.Card.Code
import Marvel.Damage
import Marvel.Entity
import Marvel.Event.Cards qualified as Cards
import Marvel.Event.Types
import Marvel.Matchers
import Marvel.Message.Lifted
import Marvel.Modifier
import Marvel.Ref

turnTheTide :: EventCard TurnTheTide
turnTheTide = event TurnTheTide Cards.turnTheTide

newtype TurnTheTide = TurnTheTide (Attrs Event)
  deriving anyclass (IsEvent, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

-- | Response (attack), after your hero thwarts and removes all threat from a scheme
-- (its window): deal 3 damage to an enemy.
instance RunMessage TurnTheTide where
  runMessage msg e@(TurnTheTide attrs) = runQueueT $ case msg of
    EventMessage ident (PlayedEvent you _ _) | ident == eventId attrs -> do
      chooseDamage you attrs (toDamage 3 $ FromPlayerAttack you) AttackableEnemy
      pure e
    _ -> TurnTheTide <$> runMessage msg attrs
