module Marvel.Upgrade.Upgrades.XGene where

import Marvel.Prelude
import Marvel.Ability
import Marvel.Card
import Marvel.Cost
import Marvel.Criteria
import Marvel.Entity
import Marvel.Matchers
import Marvel.Message.Lifted
import Marvel.Modifier
import Marvel.Payment
import Marvel.Question
import Marvel.Ref
import Marvel.Resource
import Marvel.Upgrade.Cards qualified as Cards
import Marvel.Upgrade.Types

xGene :: UpgradeCard XGene
xGene = upgrade XGene Cards.xGene

newtype XGene = XGene (Attrs Upgrade)
  deriving anyclass (IsUpgrade, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

instance HasAbilities XGene where
  getAbilities a = [ability a 1 Resource (OwnsThis <> PayingFor (HeroSpecificCard <> CardWithType EventType)) ExhaustCost (Pay $ ResourcePayment Wild)]

instance RunMessage XGene where
  runMessage msg (XGene attrs) = runQueueT $ XGene <$> runMessage msg attrs
