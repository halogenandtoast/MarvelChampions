module Marvel.Upgrade.Upgrades.HardToIgnore (
  hardToIgnore,
  HardToIgnore (..),
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
import Marvel.Question
import Marvel.Ref
import Marvel.Upgrade.Cards qualified as Cards
import Marvel.Upgrade.Types
import Marvel.Window qualified as W

hardToIgnore :: UpgradeCard HardToIgnore
hardToIgnore = upgrade HardToIgnore Cards.hardToIgnore

newtype HardToIgnore = HardToIgnore (Attrs Upgrade)
  deriving anyclass (IsUpgrade, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

-- | Hero Response: After your hero defends against an attack and takes no damage,
-- exhaust Hard to Ignore → remove 1 threat from the main scheme.
instance HasAbilities HardToIgnore where
  getAbilities (HardToIgnore a) =
    [ limitedWindowAbility
        a
        1
        (W.HeroDefendedWithoutDamage W.After $ IdentityWithId $ upgradeController a)
        HeroResponse
        OwnsThis
        ExhaustCost
        (runAbility a 1)
    ]

instance RunMessage HardToIgnore where
  runMessage msg u@(HardToIgnore attrs) = runQueueT $ case msg of
    RanAbility you (isTarget attrs -> True) 1 _ _ -> do
      removeThreat you attrs 1 MainScheme
      pure u
    _ -> HardToIgnore <$> runMessage msg attrs
