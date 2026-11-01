module Marvel.Event.Events.SwingingWebPig where

import Marvel.Prelude
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

swingingWebPig :: EventCard SwingingWebPig
swingingWebPig = event SwingingWebPig Cards.swingingWebPig

newtype SwingingWebPig = SwingingWebPig (Attrs Event)
  deriving anyclass (IsEvent, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

instance RunMessage SwingingWebPig where
  runMessage msg e@(SwingingWebPig attrs) = runQueueT $ case msg of
    EventMessage eid (PlayedEvent you _ _) | eid == eventId attrs -> do
      enemies <- selectList AttackableEnemy
      chooseOne you [targeting (enemyRef enemy) do
        dealDamageAnd enemy attrs (toDamage 6 $ FromPlayerAttack you) (confuse enemy attrs)
        | enemy <- enemies]
      pure e
    _ -> SwingingWebPig <$> runMessage msg attrs
