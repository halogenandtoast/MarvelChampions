module Marvel.Event.Events.FullBodyCharge where

import Marvel.Prelude
import Marvel.Ability
import Marvel.Card.Code
import Marvel.Entity
import Marvel.Message.Lifted
import Marvel.Modifier
import Marvel.Ref
import Marvel.Event.Cards qualified as Cards
import Marvel.Event.Types
import Marvel.Damage
import Marvel.Game.Source
import Marvel.Matchers
import Marvel.Query

fullBodyCharge :: EventCard FullBodyCharge
fullBodyCharge = event FullBodyCharge Cards.fullBodyCharge

newtype FullBodyCharge = FullBodyCharge (Attrs Event)
  deriving anyclass (IsEvent, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

instance RunMessage FullBodyCharge where
  runMessage msg e@(FullBodyCharge attrs) = runQueueT $ case msg of
    EventMessage eid (PlayedEvent you _ _) | eid == eventId attrs -> do
      (remaining, _, starting) <- getRemainingHitPoints you
      let damage = (toDamage 8 $ FromPlayerAttack you) {damageOverkill = 2 * remaining < starting}
      enemies <- selectList AttackableEnemy
      chooseOne you [targeting (enemyRef enemy) (dealDamage enemy attrs damage) | enemy <- enemies]
      pure e
    _ -> FullBodyCharge <$> runMessage msg attrs
