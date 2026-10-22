module Marvel.SideScheme.SideSchemes.CorneredStaff (
  corneredStaff,
  CorneredStaff (..),
) where

import Marvel.Prelude

import Marvel.Boost (boostCount)
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
      discardTopOfEncounterDeckThen n \cards -> do
        let icons = sum $ map (boostCount . cdBoostIcons . getCardDef) cards
        when (icons > 0) $ placeThreat ident icons
      pure s
    _ -> CorneredStaff <$> runMessage msg attrs
