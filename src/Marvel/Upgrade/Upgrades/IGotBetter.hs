module Marvel.Upgrade.Upgrades.IGotBetter where

import Marvel.Prelude
import Marvel.Ability
import Marvel.Card.Code
import Marvel.Entity
import Marvel.Message.Lifted
import Marvel.Modifier
import Marvel.Ref
import Marvel.Cost
import Marvel.Criteria
import Marvel.Matchers
import Marvel.Question
import Marvel.Upgrade.Cards qualified as Cards
import Marvel.Upgrade.Types
import Marvel.Window qualified as W

iGotBetter :: UpgradeCard IGotBetter
iGotBetter = upgrade IGotBetter Cards.iGotBetter

newtype IGotBetter = IGotBetter (Attrs Upgrade)
  deriving anyclass (IsUpgrade, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

instance HasAbilities IGotBetter where
  getAbilities a = [limitedWindowAbility a 1 (W.IdentityWouldBeDefeatedByEnemyAttack You) Interrupt OwnsThis NoCost (runAbility a 1)]

instance RunMessage IGotBetter where
  runMessage msg u@(IGotBetter attrs) = runQueueT $ case msg of
    RanAbility you (isTarget attrs -> True) 1 _ _ -> do
      cancelMatchingMessage (== IdentityMessage you IdentityDefeated)
      setHitPoints you 5
      ready you
      discardUpgrade (upgradeId attrs)
      pure u
    _ -> IGotBetter <$> runMessage msg attrs
