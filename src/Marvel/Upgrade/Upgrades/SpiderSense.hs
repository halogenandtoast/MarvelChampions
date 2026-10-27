module Marvel.Upgrade.Upgrades.SpiderSense (
  spiderSense,
  SpiderSense (..),
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

spiderSense :: UpgradeCard SpiderSense
spiderSense = upgrade SpiderSense Cards.spiderSense

newtype SpiderSense = SpiderSense (Attrs Upgrade)
  deriving anyclass (IsUpgrade, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

-- | Hero Interrupt: When the villain initiates an attack against you, draw 1 card.
instance HasAbilities SpiderSense where
  getAbilities a =
    [limitedWindowAbility a 1 (W.EnemyWouldAttack VillainEnemy You) HeroInterrupt OwnsThis NoCost $ runAbility a 1]

instance RunMessage SpiderSense where
  runMessage msg u@(SpiderSense attrs) = runQueueT $ case msg of
    RanAbility ident (isTarget attrs -> True) 1 _ _ -> do
      drawCards ident 1
      pure u
    _ -> SpiderSense <$> runMessage msg attrs
