module Marvel.SideScheme.SideSchemes.CrowdControl
  ( crowdControl
  , CrowdControl(..)
  ) where

import Marvel.Prelude

import Marvel.Card.Code
import Marvel.Entity
import Marvel.GameValue
import Marvel.Message
import Marvel.SideScheme.Attrs
import Marvel.SideScheme.Cards qualified as Cards
import Marvel.Source
import Marvel.Target

crowdControl :: SideSchemeCard CrowdControl
crowdControl = sideSchemeWith
  CrowdControl
  Cards.crowdControl
  (PerPlayer 2)
  (crisisL .~ True)

newtype CrowdControl = CrowdControl SideSchemeAttrs
  deriving anyclass IsSideScheme
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, Entity, IsSource, IsTarget)

instance RunMessage CrowdControl where
  runMessage msg (CrowdControl attrs) = CrowdControl <$> runMessage msg attrs
