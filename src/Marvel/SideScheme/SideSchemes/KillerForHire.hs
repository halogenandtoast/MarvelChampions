module Marvel.SideScheme.SideSchemes.KillerForHire (
  killerForHire,
  KillerForHire (..),
) where

import Marvel.Prelude

import Marvel.Card.Code
import Marvel.Entity
import Marvel.GameValue
import Marvel.Id ()
import Marvel.Message.Lifted
import Marvel.Modifier
import Marvel.SideScheme.Cards qualified as Cards
import Marvel.SideScheme.Types

killerForHire :: SideSchemeCard KillerForHire
killerForHire = sideScheme KillerForHire Cards.killerForHire (Static 3)

newtype KillerForHire = KillerForHire (Attrs SideScheme)
  deriving anyclass (IsSideScheme, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode)

-- | When Revealed: place an additional 1 threat per hero here.
instance RunMessage KillerForHire where
  runMessage msg s@(KillerForHire attrs) = runQueueT $ case msg of
    SideSchemeMessage ident RevealSideScheme | ident == sideSchemeId attrs -> do
      n <- fromIntegral <$> fromGameValue (PerPlayer 1)
      placeThreat ident n
      pure s
    _ -> KillerForHire <$> runMessage msg attrs
