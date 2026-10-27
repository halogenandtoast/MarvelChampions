module Marvel.Event.Events.HardKnocks (
  hardKnocks,
  HardKnocks (..),
) where

import Marvel.Prelude

import Marvel.Card.Code
import Marvel.Entity
import Marvel.Event.Cards qualified as Cards
import Marvel.Event.Types
import Marvel.Query
import Marvel.Matchers
import Marvel.Id
import Marvel.Damage
import Marvel.Message.Lifted
import Marvel.Modifier
import Marvel.Ref

hardKnocks :: EventCard HardKnocks
hardKnocks = event HardKnocks Cards.hardKnocks

newtype HardKnocks = HardKnocks (Attrs Event)
  deriving anyclass (IsEvent, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

-- | Hero Action (attack): Deal 4 damage to an enemy. If that enemy is defeated by this
-- attack, give your hero a tough status card. As Stealth Strike: the damage is followed
-- by a step naming the enemy, which its defeat turns into the tough status card.
instance RunMessage HardKnocks where
  runMessage msg e@(HardKnocks attrs) = runQueueT $ case msg of
    EventMessage ident (PlayedEvent you _ _) | ident == eventId attrs -> do
      enemies <- selectList AttackableEnemy
      chooseOne you [targeting enemy $ cardSpecific attrs "attack" enemy | enemy <- enemies]
      pure e
    CardSpecific (isTarget attrs -> True) "attack" (fromJSON -> Success (enemy :: EnemyId)) -> do
      let you = eventController attrs
      dealDamageThen enemy attrs (toDamage 4 $ FromPlayerAttack you) $
        cardSpecific attrs "attacked" enemy
      pure e
    VillainMessage vid VillainDefeated -> defeated (EnemyVillainId vid)
    MinionMessage mid MinionDefeated -> defeated (EnemyMinionId mid)
    CardSpecific (isTarget attrs -> True) "defeated" _ -> do
      becomeTough (eventController attrs)
      pure e
    _ -> HardKnocks <$> runMessage msg attrs
   where
    defeated enemy = do
      insteadOf (isAttacked enemy) $ cardSpecific attrs "defeated" ()
      HardKnocks <$> runMessage msg attrs
    isAttacked enemy = \case
      CardSpecific (isTarget attrs -> True) "attacked" (fromJSON -> Success enemy') -> enemy' == enemy
      _ -> False
