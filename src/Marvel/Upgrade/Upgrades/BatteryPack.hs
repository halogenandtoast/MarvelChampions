module Marvel.Upgrade.Upgrades.BatteryPack (
  batteryPack,
  BatteryPack (..),
) where

import Marvel.Prelude

import Marvel.Ability
import Marvel.Card.Code
import Marvel.Cost
import Marvel.Criteria
import Marvel.Entity
import Marvel.Matchers
import Marvel.Message.Lifted
import Marvel.Modifier
import Marvel.Query
import Marvel.Question
import Marvel.Ref
import Marvel.Trait
import Marvel.Upgrade.Cards qualified as Cards
import Marvel.Upgrade.Types

-- | Enters play with 2 charge counters; no uses keyword, so it stays with none (FAQ).
batteryPack :: UpgradeCard BatteryPack
batteryPack = upgradeWith BatteryPack Cards.batteryPack (usesL .~ 2)

newtype BatteryPack = BatteryPack (Attrs Upgrade)
  deriving anyclass (IsUpgrade, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

-- | Another Tech upgrade you control.
otherTech :: Attrs Upgrade -> UpgradeMatcher
otherTech a = UpgradeWithTrait Tech <> upgradeControlledBy (upgradeController a) <> NotUpgrade (UpgradeWithId $ upgradeId a)

-- | Action: Exhaust Battery Pack → move a charge counter from this card to another Tech
-- upgrade you control. The counter it moves is the use its cost removes.
instance HasAbilities BatteryPack where
  getAbilities (BatteryPack a) =
    [ability a 1 Action (OwnsThis <> UpgradeExists (otherTech a)) (ExhaustCost <> UseCost) $ runAbility a 1]

instance RunMessage BatteryPack where
  runMessage msg u@(BatteryPack attrs) = runQueueT $ case msg of
    RanAbility ident (isTarget attrs -> True) 1 _ _ -> do
      upgrades <- selectList $ otherTech attrs
      chooseOne ident [targeting other (addUpgradeUses other 1) | other <- upgrades]
      pure u
    _ -> BatteryPack <$> runMessage msg attrs
