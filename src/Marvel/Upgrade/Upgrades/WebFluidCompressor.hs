module Marvel.Upgrade.Upgrades.WebFluidCompressor where

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

webFluidCompressor :: UpgradeCard WebFluidCompressor
webFluidCompressor = upgrade WebFluidCompressor Cards.webFluidCompressor

newtype WebFluidCompressor = WebFluidCompressor (Attrs Upgrade)
  deriving anyclass (IsUpgrade, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

instance HasAbilities WebFluidCompressor where
  getAbilities a =
    [ limitedWindowAbility a 1 (W.MakesBasicAttack W.When You) HeroInterrupt OwnsThis ExhaustCost (runAbility a 1)
    ]

instance RunMessage WebFluidCompressor where
  runMessage msg u@(WebFluidCompressor attrs) = runQueueT $ case msg of
    RanAbility you (isTarget attrs -> True) 1 _ _ -> u <$ addToBasicPower you 2
    _ -> WebFluidCompressor <$> runMessage msg attrs
