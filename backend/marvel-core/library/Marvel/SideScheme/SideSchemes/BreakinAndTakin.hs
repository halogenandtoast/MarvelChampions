module Marvel.SideScheme.SideSchemes.BreakinAndTakin where

import Marvel.Prelude

import Marvel.Card.Code
import Marvel.Entity
import Marvel.GameValue
import Marvel.Message
import Marvel.SideScheme.Attrs
import Marvel.SideScheme.Cards qualified as Cards
import Marvel.Source
import Marvel.Target

breakinAndTakin :: SideSchemeCard BreakinAndTakin
breakinAndTakin =
  sideScheme BreakinAndTakin Cards.breakinAndTakin (Static 2) (Static 0)

newtype BreakinAndTakin = BreakinAndTakin SideSchemeAttrs
  deriving anyclass IsSideScheme
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, Entity, IsSource, IsTarget)

instance RunMessage BreakinAndTakin where
  runMessage msg (BreakinAndTakin attrs) = case msg of
    SideSchemeMessage sideSchemeId msg' | sideSchemeId == toId attrs ->
      case msg' of
        RevealSideScheme -> do
          n <- fromIntegral <$> fromGameValue (PerPlayer 1)
          pure . BreakinAndTakin $ attrs & threatL +~ n
        _ -> BreakinAndTakin <$> runMessage msg attrs
    _ -> BreakinAndTakin <$> runMessage msg attrs
