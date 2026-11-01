module Marvel.Event.Events.OuttaMyWay where

import Marvel.Prelude
import Marvel.Ability ()
import Marvel.Card
import Marvel.Cost ()
import Marvel.Criteria ()
import Marvel.Damage
import Marvel.Entity
import Marvel.Game.Source
import Marvel.Id
import Marvel.Matchers
import Marvel.Message.Lifted
import Marvel.Modifier
import Marvel.Query
import Marvel.Question ()
import Marvel.Ref
import Marvel.Resource ()
import Marvel.Stats ()
import Marvel.Keyword
import Marvel.Window qualified as W ()
import Marvel.Event.Cards qualified as Cards
import Marvel.Event.Types
import Marvel.Minion.Types qualified as Minion ()
import Marvel.Villain.Types qualified as Villain ()

outtaMyWay :: EventCard OuttaMyWay
outtaMyWay = event OuttaMyWay Cards.outtaMyWay

newtype OuttaMyWay = OuttaMyWay (Attrs Event)
  deriving anyclass (IsEvent, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

instance RunMessage OuttaMyWay where
  runMessage msg e@(OuttaMyWay attrs) = runQueueT $ case msg of
    EventMessage eid (PlayedEvent you _ _) | eid == eventId attrs -> do
      enemies <- selectList AttackableEnemy
      chooseOne you [targeting (enemyRef enemy) (cardSpecific attrs "attack" enemy) | enemy <- enemies]
      pure e
    CardSpecific (isTarget attrs -> True) "attack" (fromJSON -> Success (enemy :: EnemyId)) -> do
      keywords <- getEnemyKeywords enemy
      dealDamage enemy attrs (toDamage (if any (`elem` keywords) [Guard, Patrol] then 5 else 3) $ FromPlayerAttack $ eventController attrs)
      pure e
    _ -> OuttaMyWay <$> runMessage msg attrs
