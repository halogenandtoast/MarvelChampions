module Marvel.Support.Supports.SuperhumanLawDivision (
  superhumanLawDivision,
  SuperhumanLawDivision (..),
)
where

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
import Marvel.Resource
import Marvel.Support.Cards qualified as Cards
import Marvel.Support.Types

superhumanLawDivision :: SupportCard SuperhumanLawDivision
superhumanLawDivision = support SuperhumanLawDivision Cards.superhumanLawDivision

newtype SuperhumanLawDivision = SuperhumanLawDivision (Attrs Support)
  deriving anyclass (IsSupport, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

instance HasAbilities SuperhumanLawDivision where
  getAbilities (SuperhumanLawDivision a) =
    [ ability
        a
        1
        AlterEgoAction
        (OwnsThis <> InAlterEgoForm)
        (ExhaustCost <> ResourceCost (Just Mental))
        (RemoveThreat (toSource a) 2 ThwartableScheme)
    ]

instance RunMessage SuperhumanLawDivision where
  runMessage msg (SuperhumanLawDivision attrs) =
    SuperhumanLawDivision <$> runMessage msg attrs
