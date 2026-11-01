module Marvel.Event.Events.PrecisionStrike where

import Marvel.Prelude
import Marvel.Ability ()
import Marvel.Card
import Marvel.Cost ()
import Marvel.Criteria ()
import Marvel.Damage
import Marvel.Entity
import Marvel.Game.Source ()
import Marvel.Id
import Marvel.Matchers
import Marvel.Message.Lifted
import Marvel.Modifier
import Marvel.Query
import Marvel.Question ()
import Marvel.Ref
import Marvel.Resource ()
import Marvel.Stats ()
import Marvel.Keyword ()
import Marvel.Window qualified as W ()
import Marvel.Event.Cards qualified as Cards
import Marvel.Event.Types

precisionStrike :: EventCard PrecisionStrike
precisionStrike = event PrecisionStrike Cards.precisionStrike

newtype PrecisionStrike = PrecisionStrike (Attrs Event)
  deriving anyclass (IsEvent, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

instance RunMessage PrecisionStrike where
  runMessage msg e@(PrecisionStrike attrs) = runQueueT $ case msg of
    EventMessage eid (PlayedEvent you _ _) | eid == eventId attrs -> do
      enemies <- selectList AttackableEnemy
      chooseOne you [targeting (enemyRef enemy) $
        dealDamageThen enemy attrs (toDamage 2 $ FromPlayerAttack you) (cardSpecific attrs "attacked" enemy)
        | enemy <- enemies]
      pure e
    VillainMessage vid VillainDefeated -> defeated (EnemyVillainId vid)
    MinionMessage mid MinionDefeated -> defeated (EnemyMinionId mid)
    CardSpecific (isTarget attrs -> True) "heal" _ -> e <$ heal (eventController attrs) 2
    _ -> PrecisionStrike <$> runMessage msg attrs
   where
    defeated enemy = do
      insteadOf (\case
        CardSpecific (isTarget attrs -> True) "attacked" (fromJSON -> Success enemy') -> enemy == enemy'
        _ -> False) (cardSpecific attrs "heal" ())
      PrecisionStrike <$> runMessage msg attrs
