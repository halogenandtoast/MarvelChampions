module Marvel.SideScheme.SideSchemes.ImminentOverload
  ( imminentOverload
  , ImminentOverload(..)
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

imminentOverload :: SideSchemeCard ImminentOverload
imminentOverload =
  sideScheme ImminentOverload Cards.imminentOverload (Static 3)

newtype ImminentOverload = ImminentOverload (Attrs SideScheme)
  deriving anyclass (IsSideScheme, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsSource, IsTarget)

instance RunMessage ImminentOverload where
  runMessage msg (ImminentOverload attrs) = case msg of
    SideSchemeMessage ident msg' | ident == sideSchemeId attrs -> case msg' of
      RevealSideScheme -> do
        n <- fromIntegral <$> fromGameValue (PerPlayer 1)
        pure . ImminentOverload $ attrs & threatL +~ n
      _ -> ImminentOverload <$> runMessage msg attrs
    _ -> ImminentOverload <$> runMessage msg attrs
