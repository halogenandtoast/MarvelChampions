module Marvel.Upgrade.Upgrades.BloodRage where

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
import Marvel.Cost
import Marvel.Criteria
import Marvel.Matchers
import Marvel.Window qualified as W

bloodRage :: UpgradeCard BloodRage
bloodRage = upgrade BloodRage Cards.bloodRage

newtype BloodRage = BloodRage (Attrs Upgrade)
  deriving anyclass (IsUpgrade, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

instance HasAbilities BloodRage where
  getAbilities a = [limitedWindowAbility a 1 (W.EnemyDefeatedByBasicAttack You) Response OwnsThis
    (ExhaustCost <> DamageCost 1) (runAbility a 1)]

instance RunMessage BloodRage where
  runMessage msg a@(BloodRage attrs) = runQueueT $ case msg of
    RanAbility you (isTarget attrs -> True) 1 _ _ -> a <$ drawCards you 1
    _ -> BloodRage <$> runMessage msg attrs
