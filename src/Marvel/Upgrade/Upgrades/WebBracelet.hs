module Marvel.Upgrade.Upgrades.WebBracelet where

import Marvel.Prelude
import Marvel.Ability
import Marvel.Card
import Marvel.Cost
import Marvel.Criteria
import Marvel.Entity
import Marvel.Matchers
import Marvel.Message.Lifted
import Marvel.Modifier
import Marvel.Question (runAbility)
import Marvel.Ref
import Marvel.Upgrade.Cards qualified as Cards
import Marvel.Upgrade.Types
import Marvel.Window qualified as W

webBracelet :: UpgradeCard WebBracelet
webBracelet = upgrade WebBracelet Cards.webBracelet

newtype WebBracelet = WebBracelet (Attrs Upgrade)
  deriving anyclass (IsUpgrade, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

instance HasAbilities WebBracelet where
  getAbilities a =
    [ limited (SharedPerWindow "webBracelet" 1) $
        limitedWindowAbility a 1 (W.ResolvesEventAbility W.After You AnyCard)
          HeroResponse (OwnsThis <> InHeroForm) ExhaustCost (runAbility a 1)
    ]

instance RunMessage WebBracelet where
  runMessage msg u@(WebBracelet attrs) = runQueueT $ case msg of
    RanAbility you (isTarget attrs -> True) 1 _ _ -> u <$ drawCards you 1
    _ -> WebBracelet <$> runMessage msg attrs
