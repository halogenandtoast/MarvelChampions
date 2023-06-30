module Marvel.SideScheme.SideSchemes.ThePsycheMagnitron (
  thePsycheMagnitron,
  ThePsycheMagnitron (..),
) where

import Marvel.Prelude

import Marvel.Card.Code
import Marvel.Entity
import Marvel.GameValue
import Marvel.Message
import Marvel.Modifier
import Marvel.SideScheme.Cards qualified as Cards
import Marvel.SideScheme.Types

thePsycheMagnitron :: SideSchemeCard ThePsycheMagnitron
thePsycheMagnitron =
  sideScheme ThePsycheMagnitron Cards.thePsycheMagnitron (Static 3)

newtype ThePsycheMagnitron = ThePsycheMagnitron (Attrs SideScheme)
  deriving anyclass (IsSideScheme, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode)

instance RunMessage ThePsycheMagnitron where
  runMessage msg (ThePsycheMagnitron attrs) = case msg of
    SideSchemeMessage ident msg' | ident == sideSchemeId attrs -> case msg' of
      RevealSideScheme -> do
        n <- fromIntegral <$> fromGameValue (PerPlayer 1)
        pure . ThePsycheMagnitron $ attrs & threatL +~ n
      _ -> ThePsycheMagnitron <$> runMessage msg attrs
    _ -> ThePsycheMagnitron <$> runMessage msg attrs
