module Marvel.Upgrade.Upgrades.GambitsStaff where

import Marvel.Prelude
import Marvel.Ability
import Marvel.Card.Code
import Marvel.Cost
import Marvel.Criteria
import Marvel.Damage
import Marvel.Entity
import Marvel.Matchers
import Marvel.Message.Lifted
import Marvel.Modifier
import Marvel.Question
import Marvel.Ref
import Marvel.Upgrade.Cards qualified as Cards
import Marvel.Upgrade.Types
import Marvel.Window qualified as W

gambitsStaff :: UpgradeCard GambitsStaff
gambitsStaff = upgrade GambitsStaff Cards.gambitsStaff

newtype GambitsStaff = GambitsStaff (Attrs Upgrade)
  deriving anyclass (IsUpgrade, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

instance HasAbilities GambitsStaff where
  getAbilities a = [limitedWindowAbility a 1 (W.EnemyAttackedAnyTarget W.When AnyEnemy)
    HeroInterrupt OwnsThis ExhaustCost (runAbility a 1)]

instance RunMessage GambitsStaff where
  runMessage msg u@(GambitsStaff attrs) = runQueueT $ case msg of
    RanAbility _ (isTarget attrs -> True) 1 windows _ -> do
      let attackers = [enemy | W.EnemyAttack enemy _ <- windows] <> [enemy | W.EnemyAttackOnEnemy enemy _ <- windows]
      for_ attackers $ \enemy -> dealDamage enemy attrs (toDamage 1 FromAbility)
      pure u
    _ -> GambitsStaff <$> runMessage msg attrs
