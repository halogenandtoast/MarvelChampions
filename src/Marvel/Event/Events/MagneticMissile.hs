module Marvel.Event.Events.MagneticMissile where

import Marvel.Prelude
import Marvel.Ability ()
import Marvel.Card
import Marvel.Cost ()
import Marvel.Criteria ()
import Marvel.Damage
import Marvel.Deck ()
import Marvel.Discard ()
import Marvel.Entity
import Marvel.Game.Source
import Marvel.GameValue ()
import Marvel.Hand ()
import Marvel.Id
import Marvel.Keyword ()
import Marvel.Matchers
import Marvel.Message.Lifted
import Marvel.Modifier
import Marvel.Payment ()
import Marvel.Projection ()
import Marvel.Query
import Marvel.Question ()
import Marvel.Ref
import Marvel.Resource.Types ()
import Marvel.Stats ()
import Marvel.Status ()
import Marvel.Trait ()
import Marvel.Window ()
import Marvel.Identity.Types ()

import Marvel.Event.Cards qualified as Cards
import Marvel.Event.Types
import Marvel.Upgrade.Cards qualified as Cards
import Marvel.Upgrade.Types ()
magneticMissile :: EventCard MagneticMissile
magneticMissile = event MagneticMissile Cards.magneticMissile

newtype MagneticMissile = MagneticMissile (Attrs Event)
  deriving anyclass (IsEvent, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

instance RunMessage MagneticMissile where
  runMessage msg e@(MagneticMissile attrs) = runQueueT $ case msg of
    EventMessage eid (PlayedEvent you _ _) | eid == eventId attrs -> do
      wrapped <- selectList (UpgradeIs Cards.wrappedInMetal)
      targets <- catMaybes <$> traverse getUpgradeAttachedEnemy wrapped
      enemies <- filterM (\case EnemyMinionId mid -> selectAny (MinionWithId mid); _ -> pure False) targets
      chooseOne you [targeting (enemyRef enemy) $ removeFromPlay (enemyRef enemy) >> cardSpecific attrs "missile" () | enemy <- enemies]
      pure e
    CardSpecific (isTarget attrs -> True) "missile" _ -> do
      enemies <- selectList AnyEnemy
      chooseOne (eventController attrs) [targeting (enemyRef enemy) $ dealDamageAnd enemy attrs (toDamage 5 FromAbility) (stun enemy attrs) | enemy <- enemies]
      pure e
    _ -> MagneticMissile <$> runMessage msg attrs
