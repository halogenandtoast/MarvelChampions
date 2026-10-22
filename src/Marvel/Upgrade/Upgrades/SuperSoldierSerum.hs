module Marvel.Upgrade.Upgrades.SuperSoldierSerum (
  superSoldierSerum,
  SuperSoldierSerum (..),
) where

import Marvel.Prelude

import Marvel.Ability
import Marvel.Card.Code
import Marvel.Cost
import Marvel.Criteria
import Marvel.Entity
import Marvel.Message
import Marvel.Modifier
import Marvel.Payment
import Marvel.Question
import Marvel.Ref
import Marvel.Resource
import Marvel.Upgrade.Cards qualified as Cards
import Marvel.Upgrade.Types

superSoldierSerum :: UpgradeCard SuperSoldierSerum
superSoldierSerum = upgrade SuperSoldierSerum Cards.superSoldierSerum

newtype SuperSoldierSerum = SuperSoldierSerum (Attrs Upgrade)
  deriving anyclass (IsUpgrade, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

instance HasAbilities SuperSoldierSerum where
  getAbilities (SuperSoldierSerum a) =
    [ability a 1 Resource OwnsThis ExhaustCost (Pay $ ResourcePayment Physical)]

instance RunMessage SuperSoldierSerum where
  runMessage msg (SuperSoldierSerum attrs) = SuperSoldierSerum <$> runMessage msg attrs
