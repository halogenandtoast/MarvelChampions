module Marvel.Support.Supports.TheGoldenCity
  ( theGoldenCity
  , TheGoldenCity(..)
  ) where

import Marvel.Prelude

import Marvel.Ability
import Marvel.Card.Code
import Marvel.Entity
import Marvel.Message
import Marvel.Modifier
import Marvel.Source
import Marvel.Support.Attrs
import Marvel.Support.Cards qualified as Cards
import Marvel.Target

theGoldenCity :: SupportCard TheGoldenCity
theGoldenCity = support TheGoldenCity Cards.theGoldenCity

newtype TheGoldenCity = TheGoldenCity SupportAttrs
  deriving anyclass (IsSupport, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, Entity, IsSource, IsTarget)

instance HasAbilities TheGoldenCity where
  getAbilities _ = []

instance RunMessage TheGoldenCity where
  runMessage msg (TheGoldenCity attrs) = TheGoldenCity <$> runMessage msg attrs
