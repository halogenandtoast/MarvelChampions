module Marvel.SideScheme.SideSchemes.BreakinAndTakin where

import Marvel.Prelude

import Marvel.Card.Code
import Marvel.Entity
import Marvel.GameValue
import Marvel.Message
import Marvel.Modifier
import Marvel.SideScheme.Cards qualified as Cards
import Marvel.SideScheme.Types

breakinAndTakin :: SideSchemeCard BreakinAndTakin
breakinAndTakin = sideScheme BreakinAndTakin Cards.breakinAndTakin (Static 2)

newtype BreakinAndTakin = BreakinAndTakin (Attrs SideScheme)
  deriving anyclass (IsSideScheme, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode)

instance RunMessage BreakinAndTakin where
  runMessage msg (BreakinAndTakin attrs) = case msg of
    SideSchemeMessage ident msg' | ident == sideSchemeId attrs -> case msg' of
      RevealSideScheme -> do
        n <- fromIntegral <$> fromGameValue (PerPlayer 1)
        pure . BreakinAndTakin $ attrs & threatL +~ n
      _ -> BreakinAndTakin <$> runMessage msg attrs
    _ -> BreakinAndTakin <$> runMessage msg attrs
