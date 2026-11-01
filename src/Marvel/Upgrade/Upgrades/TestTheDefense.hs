module Marvel.Upgrade.Upgrades.TestTheDefense where

import Marvel.Prelude
import Marvel.Question (runAbility)
import Marvel.Ability
import Marvel.Card.Code
import Marvel.Entity
import Marvel.Message.Lifted
import Marvel.Modifier
import Marvel.Ref
import Marvel.Upgrade.Cards qualified as Cards
import Marvel.Upgrade.Types
import Marvel.Card
import Marvel.Cost
import Marvel.Criteria
import Marvel.Damage
import Marvel.Matchers
import Marvel.Trait qualified as Trait
import Marvel.Window qualified as W

testTheDefense :: UpgradeCard TestTheDefense
testTheDefense = upgrade TestTheDefense Cards.testTheDefense

newtype TestTheDefense = TestTheDefense (Attrs Upgrade)
  deriving anyclass (IsUpgrade, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

instance HasAbilities TestTheDefense where
  getAbilities a = [limitedWindowAbility a 1
    (W.PlaysEvent W.After You $ CardWithTrait Trait.Attack) Response OwnsThis NoCost (runAbility a 1)]

instance RunMessage TestTheDefense where
  runMessage msg a@(TestTheDefense attrs) = runQueueT $ case msg of
    RanAbility you (isTarget attrs -> True) 1 _ _ -> do
      addUpgradeUses (upgradeId attrs) 1
      when (upgradeUses attrs + 1 >= 5) do
        discardUpgrade (upgradeId attrs)
        chooseDamage you attrs (toDamage 5 FromAbility) DamageableEnemy
      pure a
    _ -> TestTheDefense <$> runMessage msg attrs
