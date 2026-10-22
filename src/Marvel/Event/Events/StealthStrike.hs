module Marvel.Event.Events.StealthStrike (
  stealthStrike,
  StealthStrike (..),
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

stealthStrike :: EventCard StealthStrike
stealthStrike = event StealthStrike Cards.stealthStrike

newtype StealthStrike = StealthStrike (Attrs Event)
  deriving anyclass (IsEvent, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

-- | The damage is followed by a step naming the enemy: should the damage defeat the
-- enemy, the step becomes removing the threat. It comes before the window after the
-- attack, so a response to the attack that defeats the enemy is not this attack's.
instance RunMessage StealthStrike where
  runMessage msg e@(StealthStrike attrs) = runQueueT $ case msg of
    EventMessage ident (PlayedEvent identityId _ _) | ident == eventId attrs -> do
      enemies <- selectList AttackableEnemy
      chooseOne identityId [targeting enemy $ cardSpecific attrs "attack" enemy | enemy <- enemies]
      pure e
    CardSpecific (isTarget attrs -> True) "attack" (fromJSON -> Success (enemy :: EnemyId)) -> do
      let you = eventController attrs
      dealDamageThen enemy attrs (toDamage 4 $ FromPlayerAttack you) $
        cardSpecific attrs "attacked" enemy
      pure e
    VillainMessage vid VillainDefeated -> defeated (EnemyVillainId vid)
    MinionMessage mid MinionDefeated -> defeated (EnemyMinionId mid)
    CardSpecific (isTarget attrs -> True) "defeated" _ -> do
      removeThreat (eventController attrs) attrs 2 ThwartableScheme
      pure e
    _ -> StealthStrike <$> runMessage msg attrs
   where
    defeated enemy = do
      insteadOf (isAttacked enemy) $ cardSpecific attrs "defeated" ()
      StealthStrike <$> runMessage msg attrs
    isAttacked enemy = \case
      CardSpecific (isTarget attrs -> True) "attacked" (fromJSON -> Success enemy') -> enemy' == enemy
      _ -> False
