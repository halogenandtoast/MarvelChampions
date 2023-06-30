module Marvel.SideScheme.SideSchemes.BombScare (
  bombScare,
  BombScare (..),
) where

import Marvel.Prelude

import Marvel.Card.Code
import Marvel.Entity
import Marvel.GameValue
import Marvel.Message
import Marvel.Modifier
import Marvel.SideScheme.Cards qualified as Cards
import Marvel.SideScheme.Types

bombScare :: SideSchemeCard BombScare
bombScare = sideScheme BombScare Cards.bombScare (Static 2)

newtype BombScare = BombScare (Attrs SideScheme)
  deriving anyclass (IsSideScheme, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode)

instance RunMessage BombScare where
  runMessage msg (BombScare attrs) = case msg of
    SideSchemeMessage ident msg' | ident == sideSchemeId attrs -> case msg' of
      RevealSideScheme -> do
        n <- fromIntegral <$> fromGameValue (PerPlayer 1)
        pure . BombScare $ attrs & threatL +~ n
      _ -> BombScare <$> runMessage msg attrs
    _ -> BombScare <$> runMessage msg attrs
