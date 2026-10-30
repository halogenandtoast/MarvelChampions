module Marvel.Upgrade.Upgrades.SpeedMetalAlloy where

import Marvel.Prelude
import Marvel.Ability
import Marvel.Cost
import Marvel.Criteria
import Marvel.Matchers
import Marvel.Question (runAbility)
import Marvel.Window qualified as W
import Marvel.Card.Code
import Marvel.Entity
import Marvel.Message.Lifted
import Marvel.Modifier
import Marvel.Ref
import Marvel.Upgrade.Cards qualified as Cards
import Marvel.Upgrade.Types

speedMetalAlloy :: UpgradeCard SpeedMetalAlloy
speedMetalAlloy = upgrade SpeedMetalAlloy Cards.speedMetalAlloy

newtype SpeedMetalAlloy = SpeedMetalAlloy (Attrs Upgrade)
  deriving anyclass (IsUpgrade, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

instance HasAbilities SpeedMetalAlloy where
  getAbilities a =
    [ limitedWindowAbility a 1 (W.HeroDefended W.When You AnyEnemy) HeroInterrupt OwnsThis ExhaustCost (runAbility a 1)
    ]

instance RunMessage SpeedMetalAlloy where
  runMessage msg u@(SpeedMetalAlloy attrs) = runQueueT $ case msg of
    RanAbility you (isTarget attrs -> True) 1 _ _ -> u <$ addToBasicPower you 2
    _ -> SpeedMetalAlloy <$> runMessage msg attrs
