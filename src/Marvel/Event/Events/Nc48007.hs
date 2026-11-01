module Marvel.Event.Events.Nc48007 where

import Marvel.Prelude
import Marvel.Ability ()
import Marvel.Card
import Marvel.Cost ()
import Marvel.Criteria ()
import Marvel.Damage
import Marvel.Entity
import Marvel.Game.Source ()
import Marvel.GameValue ()
import Marvel.Boost ()
import Marvel.Phase ()
import Marvel.Ally.Types ()
import Marvel.Minion.Types ()
import Marvel.Nightcrawler.Helpers ()
import Marvel.Minion.Cards ()
import Marvel.SideScheme.Cards ()
import Marvel.Hand ()
import Marvel.Id ()
import Marvel.Keyword ()
import Marvel.Matchers
import Marvel.Message.Lifted
import Marvel.Modifier
import Marvel.Payment ()
import Marvel.Projection ()
import Marvel.Query
import Marvel.Question ()
import Marvel.Ref
import Marvel.Resource ()
import Marvel.Stats ()
import Marvel.Status ()
import Marvel.Trait ()
import Marvel.Window ()
import Marvel.Event.Types
import Marvel.Event.Cards qualified as Cards

nc48007 :: EventCard Nc48007
nc48007 = event Nc48007 Cards.nc48007

newtype Nc48007 = Nc48007 (Attrs Event)
  deriving anyclass (IsEvent, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

instance RunMessage Nc48007 where
  runMessage msg a@(Nc48007 attrs) = runQueueT $ case msg of
    EventMessage eid (PlayedEvent you _ _) | eid == eventId attrs -> do
      chooseDamage you attrs (toDamage 3 $ FromPlayerAttack you) AttackableEnemy
      cardSpecific attrs "bamfDamage" you
      pure a
    CardSpecific (isTarget attrs -> True) "bamfDamage" (fromJSON -> Success you) -> do
      enemies <- selectList (EnemyWithUpgrade $ UpgradeNamed "Bamf!")
      dealDamageToEach enemies attrs (toDamage 3 $ FromPlayerAttack you)
      pure a
    _ -> Nc48007 <$> runMessage msg attrs
