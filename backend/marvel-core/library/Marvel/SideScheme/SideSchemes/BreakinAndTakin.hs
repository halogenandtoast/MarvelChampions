module Marvel.SideScheme.SideSchemes.BreakinAndTakin where

import Marvel.Prelude

import Marvel.Card.Code
import Marvel.Entity
import Marvel.Message
import Marvel.SideScheme.Attrs
import Marvel.SideScheme.Cards qualified as Cards
import Marvel.Source
import Marvel.Target

breakinAndTakin :: SideSchemeCard BreakinAndTakin
breakinAndTakin = sideScheme BreakinAndTakin Cards.breakinAndTakin

newtype BreakinAndTakin = BreakinAndTakin SideSchemeAttrs
  deriving anyclass IsSideScheme
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, Entity, IsSource, IsTarget)

instance RunMessage BreakinAndTakin where
  runMessage msg (BreakinAndTakin attrs) =
    BreakinAndTakin <$> runMessage msg attrs
