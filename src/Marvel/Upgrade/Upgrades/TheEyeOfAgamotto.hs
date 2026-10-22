module Marvel.Upgrade.Upgrades.TheEyeOfAgamotto (
  theEyeOfAgamotto,
  TheEyeOfAgamotto (..),
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

theEyeOfAgamotto :: UpgradeCard TheEyeOfAgamotto
theEyeOfAgamotto = upgrade TheEyeOfAgamotto Cards.theEyeOfAgamotto

newtype TheEyeOfAgamotto = TheEyeOfAgamotto (Attrs Upgrade)
  deriving anyclass (IsUpgrade, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

instance HasAbilities TheEyeOfAgamotto where
  getAbilities (TheEyeOfAgamotto a) =
    [ability a 1 HeroResource OwnsThis ExhaustCost (Pay $ ResourcePayment Wild)]

instance RunMessage TheEyeOfAgamotto where
  runMessage msg (TheEyeOfAgamotto attrs) = TheEyeOfAgamotto <$> runMessage msg attrs
