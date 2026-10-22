module Marvel.Upgrade.Upgrades.BlackWidowsGauntlet (
  blackWidowsGauntlet,
  BlackWidowsGauntlet (..),
) where

import Marvel.Prelude

import Marvel.Ability
import Marvel.Card
import Marvel.Cost
import Marvel.Criteria
import Marvel.Entity
import Marvel.Message
import Marvel.Modifier
import Marvel.Payment
import Marvel.Question
import Marvel.Ref
import Marvel.Resource
import Marvel.Trait
import Marvel.Upgrade.Cards qualified as Cards
import Marvel.Upgrade.Types

blackWidowsGauntlet :: UpgradeCard BlackWidowsGauntlet
blackWidowsGauntlet = upgrade BlackWidowsGauntlet Cards.blackWidowsGauntlet

newtype BlackWidowsGauntlet = BlackWidowsGauntlet (Attrs Upgrade)
  deriving anyclass (IsUpgrade, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

instance HasAbilities BlackWidowsGauntlet where
  getAbilities a =
    [ ability a 1 Resource OwnsThis ExhaustCost $
        Pay (ResourcePaymentFor (CardWithTrait Preparation) Wild)
    ]

instance RunMessage BlackWidowsGauntlet where
  runMessage msg (BlackWidowsGauntlet attrs) = BlackWidowsGauntlet <$> runMessage msg attrs
