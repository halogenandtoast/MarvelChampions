module Marvel.Upgrade.Upgrades.KarmicStaff (
  karmicStaff,
  KarmicStaff (..),
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

karmicStaff :: UpgradeCard KarmicStaff
karmicStaff = upgrade KarmicStaff Cards.karmicStaff

newtype KarmicStaff = KarmicStaff (Attrs Upgrade)
  deriving anyclass (IsUpgrade, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

-- | Resource: Exhaust Karmic Staff → generate a [wild] resource.
instance HasAbilities KarmicStaff where
  getAbilities (KarmicStaff a) =
    [ability a 1 Resource OwnsThis ExhaustCost (Pay $ ResourcePayment Wild)]

instance RunMessage KarmicStaff where
  runMessage msg (KarmicStaff attrs) = KarmicStaff <$> runMessage msg attrs
