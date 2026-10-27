module Marvel.Upgrade.Upgrades.FollowThrough (
  followThrough,
  FollowThrough (..),
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
import Marvel.Window

followThrough :: UpgradeCard FollowThrough
followThrough = upgrade FollowThrough Cards.followThrough

newtype FollowThrough = FollowThrough (Attrs Upgrade)
  deriving anyclass (IsUpgrade, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

-- | "Hero Interrupt: When your hero's attack deals any amount of excess damage, increase
-- that amount by 1." A hero attack's damage, its overkill too; each copy answers each
-- instance once.
instance HasAbilities FollowThrough where
  getAbilities a =
    [ limitedWindowAbility a 1 (HeroAttackDealsExcessDamage You) HeroInterrupt OwnsThis NoCost $
        runAbility a 1
    ]

instance RunMessage FollowThrough where
  runMessage msg u@(FollowThrough attrs) = runQueueT $ case msg of
    RanAbility _ (isTarget attrs -> True) 1 _ _ -> do
      increaseExcessDamage 1
      pure u
    _ -> FollowThrough <$> runMessage msg attrs
