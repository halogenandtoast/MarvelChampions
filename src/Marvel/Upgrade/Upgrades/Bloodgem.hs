module Marvel.Upgrade.Upgrades.Bloodgem where

import Marvel.Prelude
import Marvel.Question (Choice (Pay))
import Marvel.Ability
import Marvel.Card.Code
import Marvel.Entity
import Marvel.Message.Lifted
import Marvel.Modifier
import Marvel.Ref
import Marvel.Upgrade.Cards qualified as Cards
import Marvel.Upgrade.Types
import Marvel.Cost
import Marvel.Criteria
import Marvel.Resource.Types (Resource (Wild))
import Marvel.Payment

bloodgem :: UpgradeCard Bloodgem
bloodgem = upgrade Bloodgem Cards.bloodgem

newtype Bloodgem = Bloodgem (Attrs Upgrade)
  deriving anyclass (IsUpgrade, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

instance HasAbilities Bloodgem where
  getAbilities a = [ability a 1 Resource OwnsThis (ExhaustCost <> DamageCost 2) (Pay $ ResourcePayment Wild)]

instance RunMessage Bloodgem where
  runMessage msg (Bloodgem attrs) = runQueueT $ Bloodgem <$> runMessage msg attrs
