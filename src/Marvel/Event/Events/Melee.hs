module Marvel.Event.Events.Melee (
  melee,
  Melee (..),
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

melee :: EventCard Melee
melee = event Melee Cards.melee

newtype Melee = Melee (Attrs Event)
  deriving anyclass (IsEvent, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

-- | The second enemy is chosen once the first has been dealt its damage: a guard
-- defeated by it no longer keeps the villain from being chosen.
instance RunMessage Melee where
  runMessage msg e@(Melee attrs) = runQueueT $ case msg of
    EventMessage ident msg' | ident == eventId attrs -> case msg' of
      PlayedEvent identityId _ _ -> do
        enemies <- selectList AttackableEnemy
        chooseOne identityId
          [ targeting enemy do
              dealDamage enemy attrs (toDamage 3 $ FromPlayerAttack identityId)
              cardSpecific attrs "another" (identityId, enemy)
          | enemy <- enemies
          ]
        pure e
      _ -> Melee <$> runMessage msg attrs
    CardSpecific (isTarget attrs -> True) "another" (fromJSON -> Success (identityId, first)) -> do
      enemies <- selectList $ AttackableEnemy <> NotEnemy (EnemyWithId first)
      chooseOne identityId
        [targeting (enemyRef enemy) $ dealDamage enemy attrs (toDamage 3 $ FromPlayerAttack identityId) | enemy <- enemies]
      pure e
    _ -> Melee <$> runMessage msg attrs
