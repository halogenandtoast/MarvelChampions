module Marvel.Event.Events.Hogwashed where

import Marvel.Prelude
import Marvel.Id
import Marvel.Damage
import Marvel.Matchers
import Marvel.Query
import Marvel.Ability ()
import Marvel.Card.Code
import Marvel.Entity
import Marvel.Message.Lifted
import Marvel.Modifier
import Marvel.Ref
import Marvel.Event.Cards qualified as Cards
import Marvel.Event.Types

hogwashed :: EventCard Hogwashed
hogwashed = event Hogwashed Cards.hogwashed

newtype Hogwashed = Hogwashed (Attrs Event)
  deriving anyclass (IsEvent, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

instance RunMessage Hogwashed where
  runMessage msg e@(Hogwashed attrs) = runQueueT $ case msg of
    EventMessage eid (PlayedEvent you _ _) | eid == eventId attrs -> do
      minions <- selectList AnyMinion
      schemes <- selectList (NotScheme MainScheme <> ThwartableScheme)
      chooseOne you $
        [targeting (enemyRef $ EnemyMinionId m) (dealDamage m attrs $ toDamage 5 FromAbility) | m <- minions]
        <> [targeting (schemeRef s) (thwartScheme s attrs 5) | s <- schemes]
      pure e
    _ -> Hogwashed <$> runMessage msg attrs
