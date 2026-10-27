module Marvel.SideScheme.SideSchemes.Vendetta (
  vendetta,
  Vendetta (..),
) where

import Marvel.Prelude

import Marvel.Ref
import Marvel.Card.Code
import Marvel.Entity
import Marvel.GameValue
import Marvel.Message.Lifted
import Marvel.Modifier
import Marvel.SideScheme.Cards qualified as Cards
import Marvel.SideScheme.Types

-- | Base threat 2 per player, and no text: its amplify icon is what it does.
vendetta :: SideSchemeCard Vendetta
vendetta = sideScheme Vendetta Cards.vendetta (PerPlayer 2)

newtype Vendetta = Vendetta (Attrs SideScheme)
  deriving anyclass (IsSideScheme, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode)

instance RunMessage Vendetta where
  runMessage msg (Vendetta attrs) = runQueueT $ Vendetta <$> runMessage msg attrs
