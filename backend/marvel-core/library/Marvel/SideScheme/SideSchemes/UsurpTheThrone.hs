module Marvel.SideScheme.SideSchemes.UsurpTheThrone (
  usurpTheThrone,
  UsurpTheThrone (..),
) where

import Marvel.Prelude

import Marvel.Card.Code
import Marvel.Entity
import Marvel.GameValue
import Marvel.Message
import Marvel.Modifier
import Marvel.SideScheme.Cards qualified as Cards
import Marvel.SideScheme.Types

usurpTheThrone :: SideSchemeCard UsurpTheThrone
usurpTheThrone = sideScheme UsurpTheThrone Cards.usurpTheThrone (PerPlayer 3)

newtype UsurpTheThrone = UsurpTheThrone (Attrs SideScheme)
  deriving anyclass (IsSideScheme, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode)

instance RunMessage UsurpTheThrone where
  runMessage msg (UsurpTheThrone attrs) =
    UsurpTheThrone <$> runMessage msg attrs
