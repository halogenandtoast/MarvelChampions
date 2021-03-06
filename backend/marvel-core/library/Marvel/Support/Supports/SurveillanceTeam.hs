module Marvel.Support.Supports.SurveillanceTeam
  ( surveillanceTeam
  , SurveillanceTeam(..)
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
import Marvel.Source
import Marvel.Support.Attrs
import Marvel.Support.Cards qualified as Cards
import Marvel.Target

surveillanceTeam :: SupportCard SurveillanceTeam
surveillanceTeam =
  supportWith SurveillanceTeam Cards.surveillanceTeam
    $ (usesL .~ 3)
    . (discardIfNoUsesL .~ True)

newtype SurveillanceTeam = SurveillanceTeam SupportAttrs
  deriving anyclass (IsSupport, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, Entity, IsSource, IsTarget)

instance HasAbilities SurveillanceTeam where
  getAbilities (SurveillanceTeam a) =
    [ ability
        a
        1
        Action
        (OwnsThis <> SchemeExists ThwartableScheme)
        (ExhaustCost <> UseCost)
        (RemoveThreat (toSource a) 1 ThwartableScheme)
    ]

instance RunMessage SurveillanceTeam where
  runMessage msg (SurveillanceTeam attrs) =
    SurveillanceTeam <$> runMessage msg attrs
