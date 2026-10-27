module Marvel.Upgrade.Upgrades.MountedLaser (
  mountedLaser,
  MountedLaser (..),
) where

import Marvel.Prelude

import Marvel.Ability
import Marvel.Card.Code
import Marvel.Cost
import Marvel.Criteria
import Marvel.Damage
import Marvel.Entity
import Marvel.Game.Source
import Marvel.Matchers
import Marvel.Message.Lifted
import Marvel.Modifier
import Marvel.Question
import Marvel.Ref
import Marvel.Upgrade.Cards qualified as Cards
import Marvel.Upgrade.Types

mountedLaser :: UpgradeCard MountedLaser
mountedLaser = upgrade MountedLaser Cards.mountedLaser

newtype MountedLaser = MountedLaser (Attrs Upgrade)
  deriving anyclass (IsUpgrade, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

-- | Hero Action: Exhaust Mounted Laser → deal 2 damage to an enemy (3 damage instead if
-- you control the Milano). Not an attack: no retaliate, and any enemy may be chosen.
instance HasAbilities MountedLaser where
  getAbilities a =
    [ability a 1 HeroAction (OwnsThis <> EnemyExists DamageableEnemy) ExhaustCost (runAbility a 1)]

instance RunMessage MountedLaser where
  runMessage msg u@(MountedLaser attrs) = runQueueT $ case msg of
    RanAbility you (isTarget attrs -> True) 1 _ _ -> do
      milano <- controlsMilano you
      chooseDamage you attrs (toDamage (if milano then 3 else 2) FromAbility) AnyEnemy
      pure u
    _ -> MountedLaser <$> runMessage msg attrs
