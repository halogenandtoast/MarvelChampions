module Marvel.Support.Supports.SuperhumanLawDivision
  ( superhumanLawDivision
  , SuperhumanLawDivision(..)
  )
where

import Marvel.Prelude

import Marvel.Ability
import Marvel.Cost
import Marvel.Card.Code
import Marvel.Criteria
import Marvel.Entity
import Marvel.Matchers
import Marvel.Message
import Marvel.Modifier
import Marvel.Question
import Marvel.Resource
import Marvel.Source
import Marvel.Support.Attrs
import Marvel.Support.Cards qualified as Cards
import Marvel.Target

superhumanLawDivision :: SupportCard SuperhumanLawDivision
superhumanLawDivision = support SuperhumanLawDivision Cards.superhumanLawDivision

newtype SuperhumanLawDivision = SuperhumanLawDivision SupportAttrs
  deriving anyclass (IsSupport, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, Entity, IsSource, IsTarget)

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
