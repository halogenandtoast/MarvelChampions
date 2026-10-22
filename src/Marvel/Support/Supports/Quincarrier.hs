module Marvel.Support.Supports.Quincarrier (
  quincarrier,
  Quincarrier (..),
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
import Marvel.Support.Cards qualified as Cards
import Marvel.Support.Types

quincarrier :: SupportCard Quincarrier
quincarrier = support Quincarrier Cards.quincarrier

newtype Quincarrier = Quincarrier (Attrs Support)
  deriving anyclass (IsSupport, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

instance HasAbilities Quincarrier where
  getAbilities a =
    [ability a 1 Resource OwnsThis ExhaustCost $ Pay (ResourcePayment Wild)]

instance RunMessage Quincarrier where
  runMessage msg (Quincarrier attrs) = Quincarrier <$> runMessage msg attrs
