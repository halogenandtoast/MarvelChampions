module Marvel.Support.Supports.NebulasShipSupport (nebulasShipSupport, NebulasShipSupport (..)) where

import Marvel.Prelude

import Marvel.Card.Code
import Marvel.Ability
import Marvel.Cost.Types
import Marvel.Criteria
import Marvel.Entity
import Marvel.Message.Lifted
import Marvel.Modifier
import Marvel.Payment
import Marvel.Question
import Marvel.Ref
import Marvel.Resource
import Marvel.Support.Cards qualified as Cards
import Marvel.Support.Types

nebulasShipSupport :: SupportCard NebulasShipSupport
nebulasShipSupport = support NebulasShipSupport Cards.nebulasShipSupport

newtype NebulasShipSupport = NebulasShipSupport (Attrs Support)
  deriving anyclass (IsSupport, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

instance HasAbilities NebulasShipSupport where
  getAbilities a = [ability a 1 Resource OwnsThis ExhaustCost $ Pay (ResourcePayment Wild)]

instance RunMessage NebulasShipSupport where
  runMessage msg (NebulasShipSupport attrs) = runQueueT $ NebulasShipSupport <$> runMessage msg attrs
