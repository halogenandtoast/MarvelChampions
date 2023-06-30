module Marvel.SideScheme.SideSchemes.IllegalArmsFactory (
  illegalArmsFactory,
  IllegalArmsFactory (..),
) where

import Marvel.Prelude

import Marvel.Card.Code
import Marvel.Entity
import Marvel.GameValue
import Marvel.Message
import Marvel.Modifier
import Marvel.SideScheme.Cards qualified as Cards
import Marvel.SideScheme.Types

illegalArmsFactory :: SideSchemeCard IllegalArmsFactory
illegalArmsFactory =
  sideScheme IllegalArmsFactory Cards.illegalArmsFactory (PerPlayer 1)

newtype IllegalArmsFactory = IllegalArmsFactory (Attrs SideScheme)
  deriving anyclass (IsSideScheme, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode)

instance RunMessage IllegalArmsFactory where
  runMessage msg (IllegalArmsFactory attrs) = case msg of
    SideSchemeMessage ident msg' | ident == sideSchemeId attrs -> case msg' of
      RevealSideScheme -> do
        n <- fromIntegral <$> fromGameValue (PerPlayer 1)
        pure . IllegalArmsFactory $ attrs & threatL +~ n
      _ -> IllegalArmsFactory <$> runMessage msg attrs
    _ -> IllegalArmsFactory <$> runMessage msg attrs
