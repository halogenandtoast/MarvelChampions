module Marvel.Support.Supports.AvengersMansion (
  avengersMansion,
  AvengersMansion (..),
) where

import Marvel.Prelude

import Marvel.Ability
import Marvel.Card.Code
import Marvel.Cost
import Marvel.Criteria
import Marvel.Entity
import Marvel.Matchers
import Marvel.Message
import Marvel.Modifier
import Marvel.Question
import Marvel.Ref
import Marvel.Support.Cards qualified as Cards
import Marvel.Support.Types

avengersMansion :: SupportCard AvengersMansion
avengersMansion = support AvengersMansion Cards.avengersMansion

newtype AvengersMansion = AvengersMansion (Attrs Support)
  deriving anyclass (IsSupport, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

instance HasAbilities AvengersMansion where
  getAbilities a =
    [ability a 1 Action NoCriteria ExhaustCost $ ChooseDrawCards 1 AnyIdentity]

instance RunMessage AvengersMansion where
  runMessage msg (AvengersMansion attrs) =
    AvengersMansion <$> runMessage msg attrs
