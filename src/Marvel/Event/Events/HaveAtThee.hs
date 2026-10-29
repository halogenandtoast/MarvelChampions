module Marvel.Event.Events.HaveAtThee (haveAtThee, HaveAtThee (..)) where

import Marvel.Prelude
import Marvel.Card
import Marvel.Entity
import Marvel.Event.Cards qualified as Cards
import Marvel.Event.Types
import Marvel.Matchers
import Marvel.Message.Lifted
import Marvel.Modifier
import Marvel.Query
import Marvel.Ref
import Marvel.Damage
import Marvel.Upgrade.Cards qualified as Upgrades

haveAtThee :: EventCard HaveAtThee
haveAtThee = event HaveAtThee Cards.haveAtThee

newtype HaveAtThee = HaveAtThee (Attrs Event)
  deriving anyclass (IsEvent, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

instance RunMessage HaveAtThee where
  runMessage msg e@(HaveAtThee attrs) = runQueueT $ case msg of
    EventMessage eid (PlayedEvent you _ _) | eid == eventId attrs -> do
      enemies <- selectList (AttackableBy $ IdentityWithId you)
      unless (null enemies) $ chooseOne you
        [ targeting (enemyRef enemy) do
            marked <- selectAny (EnemyWithId enemy <> EnemyWithUpgrade (UpgradeIs Upgrades.deathGlow))
            let damage = (if marked then withOverkill else id) $ toDamage 7 (FromPlayerAttack you)
            dealDamage enemy attrs damage
        | enemy <- enemies
        ]
      pure e
    _ -> HaveAtThee <$> runMessage msg attrs
