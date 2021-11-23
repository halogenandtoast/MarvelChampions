module Marvel.SideScheme.SideSchemes.BombScare
  ( bombScare
  , BombScare(..)
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

bombScare :: SideSchemeCard BombScare
bombScare = sideScheme BombScare Cards.bombScare (Static 2) (Static 0)

newtype BombScare = BombScare SideSchemeAttrs
  deriving anyclass IsSideScheme
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, Entity, IsSource, IsTarget)

instance RunMessage BombScare where
  runMessage msg (BombScare attrs) = case msg of
    SideSchemeMessage sideSchemeId msg' | sideSchemeId == toId attrs ->
      case msg' of
        RevealSideScheme -> do
          n <- fromIntegral <$> fromGameValue (PerPlayer 1)
          pure . BombScare $ attrs & threatL +~ n
        _ -> BombScare <$> runMessage msg attrs
    _ -> BombScare <$> runMessage msg attrs
