module Marvel.Event.Events.AcrobaticMove (
  acrobaticMove,
  AcrobaticMove (..),
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
import Marvel.Query
import Marvel.Ref

acrobaticMove :: EventCard AcrobaticMove
acrobaticMove = event AcrobaticMove Cards.acrobaticMove

newtype AcrobaticMove = AcrobaticMove (Attrs Event)
  deriving anyclass (IsEvent, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

-- | Deal 2 damage to an enemy.
instance RunMessage AcrobaticMove where
  runMessage msg e@(AcrobaticMove attrs) = runQueueT $ case msg of
    EventMessage ident msg' | ident == eventId attrs -> case msg' of
      PlayedEvent identityId _ _ -> do
        enemies <- selectList AttackableEnemy
        chooseOne identityId
          [targeting (enemyRef enemy) $ dealDamage enemy attrs (toDamage 2 $ FromPlayerAttack identityId) | enemy <- enemies]
        pure e
      _ -> AcrobaticMove <$> runMessage msg attrs
    _ -> AcrobaticMove <$> runMessage msg attrs
