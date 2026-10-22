module Marvel.Upgrade.Upgrades.GodOfThunder (
  godOfThunder,
  GodOfThunder (..),
) where

import Marvel.Prelude

import Marvel.Ability
import Marvel.Card.Code
import Marvel.Cost
import Marvel.Criteria
import Marvel.Entity
import Marvel.Message
import Marvel.Modifier
import Marvel.Question
import Marvel.Ref
import Marvel.Resource
import Marvel.Upgrade.Cards qualified as Cards
import Marvel.Upgrade.Types

godOfThunder :: UpgradeCard GodOfThunder
godOfThunder = upgrade GodOfThunder Cards.godOfThunder

newtype GodOfThunder = GodOfThunder (Attrs Upgrade)
  deriving anyclass (IsUpgrade, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

instance HasAbilities GodOfThunder where
  getAbilities (GodOfThunder a) =
    [ability a 1 HeroResource OwnsThis ExhaustCost (Pay $ ResourcePayment Energy)]

instance RunMessage GodOfThunder where
  runMessage msg (GodOfThunder attrs) = GodOfThunder <$> runMessage msg attrs
