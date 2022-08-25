module Marvel.Support.Supports.TheGoldenCity
  ( theGoldenCity
  , TheGoldenCity(..)
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
import Marvel.Support.Types
import Marvel.Support.Cards qualified as Cards
import Marvel.Target

theGoldenCity :: SupportCard TheGoldenCity
theGoldenCity = support TheGoldenCity Cards.theGoldenCity

newtype TheGoldenCity = TheGoldenCity (Attrs Support)
  deriving anyclass (IsSupport, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsSource, IsTarget)

instance HasAbilities TheGoldenCity where
  getAbilities (TheGoldenCity a) =
    [ ability a 1 AlterEgoAction OwnsThis ExhaustCost
        $ ChooseDrawCards 2
        $ IdentityWithId (supportController a)
    ]

instance RunMessage TheGoldenCity where
  runMessage msg (TheGoldenCity attrs) = TheGoldenCity <$> runMessage msg attrs
