module Marvel.Upgrade.Upgrades.Ingenuity where

import Marvel.Prelude
import Marvel.Cost
import Marvel.Criteria
import Marvel.Payment
import Marvel.Question
import Marvel.Resource
import Marvel.Ability
import Marvel.Upgrade.Cards qualified as Cards
import Marvel.Upgrade.Types
import Marvel.Card.Code
import Marvel.Entity ()
import Marvel.GameValue ()
import Marvel.Hand ()
import Marvel.Message.Lifted
import Marvel.Modifier
import Marvel.Ref
import Marvel.Stats ()

ingenuity :: UpgradeCard Ingenuity
ingenuity = upgrade Ingenuity Cards.ingenuity

newtype Ingenuity = Ingenuity (Attrs Upgrade)
  deriving anyclass (IsUpgrade, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

instance RunMessage Ingenuity where
  runMessage msg (Ingenuity attrs) = Ingenuity <$> runMessage msg attrs

instance HasAbilities Ingenuity where
  getAbilities (Ingenuity attrs) = [ability attrs 1 Resource OwnsThis ExhaustCost (Pay $ ResourcePayment Mental)]
