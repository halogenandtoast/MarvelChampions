module Marvel.Event.Events.RootStomp (
  rootStomp,
  RootStomp (..),
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

rootStomp :: EventCard RootStomp
rootStomp = event RootStomp Cards.rootStomp

newtype RootStomp = RootStomp (Attrs Event)
  deriving anyclass (IsEvent, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

-- | Hero Action (attack): deal 5 damage to an enemy. If this attack defeats that enemy,
-- place 1 growth counter on Groot (to a maximum of 10). As Stealth Strike: the damage is
-- followed by a step naming the enemy, which its defeat turns into placing the counter.
instance RunMessage RootStomp where
  runMessage msg e@(RootStomp attrs) = runQueueT $ case msg of
    EventMessage ident (PlayedEvent you _ _) | ident == eventId attrs -> do
      enemies <- selectList AttackableEnemy
      chooseOne you [targeting (enemyRef enemy) $ cardSpecific attrs "attack" enemy | enemy <- enemies]
      pure e
    CardSpecific (isTarget attrs -> True) "attack" (fromJSON -> Success (enemy :: EnemyId)) -> do
      let you = eventController attrs
      dealDamageThen enemy attrs (toDamage 5 $ FromPlayerAttack you) $
        cardSpecific attrs "attacked" enemy
      pure e
    VillainMessage vid VillainDefeated -> defeated (EnemyVillainId vid)
    MinionMessage mid MinionDefeated -> defeated (EnemyMinionId mid)
    CardSpecific (isTarget attrs -> True) "defeated" _ -> do
      placeGrowthCounters (eventController attrs) 1
      pure e
    _ -> RootStomp <$> runMessage msg attrs
   where
    defeated enemy = do
      insteadOf (isAttacked enemy) $ cardSpecific attrs "defeated" ()
      RootStomp <$> runMessage msg attrs
    isAttacked enemy = \case
      CardSpecific (isTarget attrs -> True) "attacked" (fromJSON -> Success enemy') -> enemy' == enemy
      _ -> False
