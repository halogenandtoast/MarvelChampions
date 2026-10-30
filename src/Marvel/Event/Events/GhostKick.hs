module Marvel.Event.Events.GhostKick where

import Marvel.Prelude
import Marvel.Card
import Marvel.Damage
import Marvel.Entity
import Marvel.Event.Cards qualified as Cards
import Marvel.Event.Types
import Marvel.Matchers
import Marvel.Message.Lifted
import Marvel.Modifier
import Marvel.Query
import Marvel.Ref

ghostKick :: EventCard GhostKick
ghostKick = event GhostKick Cards.ghostKick

newtype GhostKick = GhostKick (Attrs Event)
  deriving anyclass (IsEvent, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

instance RunMessage GhostKick where
  runMessage msg e@(GhostKick attrs) = runQueueT $ case msg of
    EventMessage eid (PlayedEvent you _ _) | eid == eventId attrs -> do
      enemies <- selectList AttackableEnemy
      unless (null enemies) $ chooseOne you
        [targeting (enemyRef enemy) (dealDamage enemy attrs $ toDamage 6 $ FromPlayerAttack you) | enemy <- enemies]
      pure e
    _ -> GhostKick <$> runMessage msg attrs
