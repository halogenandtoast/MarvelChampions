module Marvel.Event.Events.SwingingSilkKickSilk where
import Marvel.Prelude
import Marvel.Query
import Marvel.Id
import Marvel.Cost ()
import Marvel.Criteria ()
import Marvel.Matchers
import Marvel.Question ()
import Marvel.Window qualified as W ()
import Marvel.Trait qualified as T ()
import Marvel.Damage
import Marvel.Game.Source ()
import Marvel.Card
import Marvel.Card.Id ()
import Marvel.Silk.Helpers

import Marvel.Ability ()
import Marvel.Card.Code ()
import Marvel.Entity
import Marvel.Message.Lifted
import Marvel.Modifier
import Marvel.Ref
import Marvel.Silk.Cards qualified as Cards
import Marvel.Event.Types

swingingSilkKickSilk :: EventCard SwingingSilkKickSilk
swingingSilkKickSilk = event SwingingSilkKickSilk Cards.swingingSilkKickSilk
newtype SwingingSilkKickSilk = SwingingSilkKickSilk (Attrs Event)
  deriving anyclass (IsEvent, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)
instance RunMessage SwingingSilkKickSilk where
  runMessage msg u@(SwingingSilkKickSilk attrs) = runQueueT $ case msg of
    EventMessage eid (PlayedEvent you _ _) | eid == eventId attrs -> do
      enemies <- selectList AttackableEnemy
      chooseOne you [targeting (enemyRef enemy) (cardSpecific attrs "target" (you,enemy)) | enemy <- enemies]
      pure u
    CardSpecific (isTarget attrs -> True) "target" (fromJSON -> Success (you,enemy :: EnemyId)) -> do
      cards <- matchingTuckedCards you (enemyRef enemy)
      chooseOne you (labeled "$label.attackWithoutDiscarding" (dealDamage enemy attrs $ toDamage 7 $ FromPlayerAttack you) : [targeting card (discardTuckedEncounterCard you card >> dealDamage enemy attrs ((toDamage 9 $ FromPlayerAttack you) {damageOverkill = True})) | card <- cards])
      pure u
    _ -> SwingingSilkKickSilk <$> runMessage msg attrs
