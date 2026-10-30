module Marvel.Upgrade.Upgrades.HostSpider where

import Marvel.Prelude
import Marvel.Ability
import Marvel.Cost
import Marvel.Criteria
import Marvel.Matchers qualified as Matchers
import Marvel.Question (runAbility)
import Marvel.Card.Code
import Marvel.Entity
import Marvel.Message.Lifted
import Marvel.Modifier
import Marvel.Ref
import Marvel.Upgrade.Cards qualified as Cards
import Marvel.Upgrade.Types

hostSpider :: UpgradeCard HostSpider
hostSpider = upgrade HostSpider Cards.hostSpider

newtype HostSpider = HostSpider (Attrs Upgrade)
  deriving anyclass (IsUpgrade, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

instance HasAbilities HostSpider where
  getAbilities a = [ability a 1 HeroAction (OwnsThis <> SelfMatches Matchers.ExhaustedIdentity) ExhaustCost (runAbility a 1)]

instance RunMessage HostSpider where
  runMessage msg u@(HostSpider attrs) = runQueueT $ case msg of
    RanAbility you (isTarget attrs -> True) 1 _ _ -> u <$ ready you
    _ -> HostSpider <$> runMessage msg attrs
