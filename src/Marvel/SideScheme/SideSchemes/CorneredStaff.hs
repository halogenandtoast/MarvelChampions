module Marvel.SideScheme.SideSchemes.CorneredStaff (
  corneredStaff,
  CorneredStaff (..),
) where

import Marvel.Prelude

import Marvel.Card
import Marvel.Entity
import Marvel.GameValue
import Marvel.Message.Lifted
import Marvel.Modifier
import Marvel.SideScheme.Cards qualified as Cards
import Marvel.SideScheme.Types

corneredStaff :: SideSchemeCard CorneredStaff
corneredStaff = sideSchemeWith CorneredStaff Cards.corneredStaff (Static 3) (crisisL .~ True)

newtype CorneredStaff = CorneredStaff (Attrs SideScheme)
  deriving anyclass (IsSideScheme, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode)

-- | When Revealed: discard 1[per_hero] cards from the encounter deck; 1 more threat here
-- for each boost icon discarded.
instance RunMessage CorneredStaff where
  runMessage msg s@(CorneredStaff attrs) = runQueueT $ case msg of
    SideSchemeMessage ident RevealSideScheme | ident == sideSchemeId attrs -> do
      n <- fromIntegral <$> fromGameValue (PerPlayer 1)
      discardTopOfEncounterDeckThen n (countBoostIcons attrs ())
      pure s
    WithBoostIcons (isTarget attrs -> True) _ (sum . map snd -> icons) -> do
      when (icons > 0) $ placeThreat (sideSchemeId attrs) icons
      pure s
    _ -> CorneredStaff <$> runMessage msg attrs
