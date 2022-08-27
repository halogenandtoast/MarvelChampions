module Marvel.SideScheme.SideSchemes.CrowdControl
  ( crowdControl
  , CrowdControl(..)
  ) where

import Marvel.Prelude

import Marvel.Card.Code
import Marvel.Entity
import Marvel.GameValue
import Marvel.Message
import Marvel.Modifier
import Marvel.SideScheme.Cards qualified as Cards
import Marvel.SideScheme.Types
import Marvel.Source
import Marvel.Target

crowdControl :: SideSchemeCard CrowdControl
crowdControl =
  sideSchemeWith CrowdControl Cards.crowdControl (PerPlayer 2) (crisisL .~ True)

newtype CrowdControl = CrowdControl (Attrs SideScheme)
  deriving anyclass (IsSideScheme, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsSource, IsTarget)

instance RunMessage CrowdControl where
  runMessage msg (CrowdControl attrs) = CrowdControl <$> runMessage msg attrs
