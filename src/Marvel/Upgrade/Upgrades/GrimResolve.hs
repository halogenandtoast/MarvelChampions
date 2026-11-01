module Marvel.Upgrade.Upgrades.GrimResolve where

import Marvel.Prelude
import Marvel.Cost
import Marvel.Criteria
import Marvel.Payment
import Marvel.Resource
import Marvel.Question (Choice (Pay))
import Marvel.Ability
import Marvel.Card.Code
import Marvel.Entity
import Marvel.Message.Lifted
import Marvel.Modifier
import Marvel.Ref
import Marvel.Upgrade.Cards qualified as Cards
import Marvel.Upgrade.Types

grimResolve :: UpgradeCard GrimResolve
grimResolve = upgrade GrimResolve Cards.grimResolve

newtype GrimResolve = GrimResolve (Attrs Upgrade)
  deriving anyclass (IsUpgrade, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

instance HasAbilities GrimResolve where
  getAbilities a = [ability a 1 Resource OwnsThis (ExhaustCost <> DamageCost 1) (Pay $ ResourcePayment Wild)]

instance RunMessage GrimResolve where
  runMessage msg (GrimResolve attrs) = runQueueT $ GrimResolve <$> runMessage msg attrs
