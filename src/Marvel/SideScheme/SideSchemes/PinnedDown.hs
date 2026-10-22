module Marvel.SideScheme.SideSchemes.PinnedDown (
  pinnedDown,
  PinnedDown (..),
) where

import Marvel.Prelude

import Marvel.Card
import Marvel.Entity
import Marvel.Game.Source
import Marvel.GameValue
import Marvel.Message.Lifted
import Marvel.Modifier
import Marvel.SideScheme.Cards qualified as Cards
import Marvel.SideScheme.Types

-- | A crisis.
pinnedDown :: SideSchemeCard PinnedDown
pinnedDown = sideSchemeWith PinnedDown Cards.pinnedDown (Static 2) (crisisL .~ True)

newtype PinnedDown = PinnedDown (Attrs SideScheme)
  deriving anyclass (IsSideScheme, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode)

-- | When Revealed: place 2 threat here for each obligation in play (in its game area).
instance RunMessage PinnedDown where
  runMessage msg (PinnedDown attrs) = runQueueT $ case msg of
    SideSchemeMessage ident RevealSideScheme | ident == sideSchemeId attrs -> do
      attrs' <- runMessage msg attrs
      n <- length <$> (getObligationsIn =<< getAreaOfTarget (sideSchemeId attrs))
      when (n > 0) $ placeThreat (sideSchemeId attrs) (2 * fromIntegral n)
      pure $ PinnedDown attrs'
    _ -> PinnedDown <$> runMessage msg attrs
