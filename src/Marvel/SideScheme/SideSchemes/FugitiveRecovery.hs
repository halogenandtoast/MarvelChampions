module Marvel.SideScheme.SideSchemes.FugitiveRecovery (
  fugitiveRecovery,
  FugitiveRecovery (..),
) where

import Marvel.Prelude

import Marvel.Card.Code
import Marvel.Entity
import Marvel.GameValue
import Marvel.Message.Lifted
import Marvel.Modifier
import Marvel.SideScheme.Cards qualified as Cards
import Marvel.SideScheme.Types

-- | Base threat 2; surge and Hinder 3 per player are its card's (Badoon Headhunter).
fugitiveRecovery :: SideSchemeCard FugitiveRecovery
fugitiveRecovery = sideScheme FugitiveRecovery Cards.fugitiveRecovery (Static 2)

newtype FugitiveRecovery = FugitiveRecovery (Attrs SideScheme)
  deriving anyclass (IsSideScheme, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode)

instance RunMessage FugitiveRecovery where
  runMessage msg (FugitiveRecovery attrs) = runQueueT $ FugitiveRecovery <$> runMessage msg attrs
