module Marvel.Event.Events.LungingStrike where

import Marvel.Prelude
import Marvel.Card.Code
import Marvel.Damage
import Marvel.Entity
import Marvel.Game.Source (getModifiers)
import Marvel.Event.Cards qualified as Cards
import Marvel.Event.Types
import Marvel.Matchers
import Marvel.Message.Lifted
import Marvel.Modifier
import Marvel.Ref

lungingStrike :: EventCard LungingStrike
lungingStrike = event LungingStrike Cards.lungingStrike

newtype LungingStrike = LungingStrike (Attrs Event)
  deriving anyclass (IsEvent, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

instance RunMessage LungingStrike where
  runMessage msg e@(LungingStrike attrs) = runQueueT $ case msg of
    EventMessage eid (PlayedEvent you _ _) | eid == eventId attrs -> do
      claws <- elem PlayedUsingWolverinesClaws <$> getModifiers attrs
      let dmg = (if claws then withOverkill else id) $ toDamage 8 $ FromPlayerAttack you
      chooseDamage you attrs dmg AttackableEnemy
      pure e
    _ -> LungingStrike <$> runMessage msg attrs
