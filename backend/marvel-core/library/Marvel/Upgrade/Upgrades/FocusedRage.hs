module Marvel.Upgrade.Upgrades.FocusedRage
  ( focusedRage
  , FocusedRage(..)
  ) where

import Marvel.Prelude

import Marvel.Ability
import Marvel.Card.Code
import Marvel.Cost
import Marvel.Criteria
import Marvel.Entity
import Marvel.Matchers
import Marvel.Message
import Marvel.Modifier
import Marvel.Question
import Marvel.Source
import Marvel.Target
import Marvel.Upgrade.Cards qualified as Cards
import Marvel.Upgrade.Types

focusedRage :: UpgradeCard FocusedRage
focusedRage = upgrade FocusedRage Cards.focusedRage

newtype FocusedRage = FocusedRage (Attrs Upgrade)
  deriving anyclass (IsUpgrade, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsSource, IsTarget)

instance HasAbilities FocusedRage where
  getAbilities (FocusedRage a) =
    [ ability
        a
        1
        HeroAction
        (OwnsThis <> InHeroForm)
        (ExhaustCost <> DamageCost 1)
        (ChooseDrawCards 1 You)
    ]

instance RunMessage FocusedRage where
  runMessage msg (FocusedRage attrs) = FocusedRage <$> runMessage msg attrs
