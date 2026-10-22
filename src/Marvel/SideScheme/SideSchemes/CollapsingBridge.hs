module Marvel.SideScheme.SideSchemes.CollapsingBridge (
  collapsingBridge,
  CollapsingBridge (..),
) where

import Marvel.Prelude

import Marvel.Card.Code
import Marvel.Entity
import Marvel.GameValue
import Marvel.Message.Lifted
import Marvel.Modifier
import Marvel.Ref
import Marvel.SideScheme.Cards qualified as Cards
import Marvel.SideScheme.Types
import Marvel.Environment.Environments.CriminalEnterprise (infamyOrMadness)

collapsingBridge :: SideSchemeCard CollapsingBridge
collapsingBridge = sideSchemeWith CollapsingBridge Cards.collapsingBridge (PerPlayer 2) (crisisL .~ True)

newtype CollapsingBridge = CollapsingBridge (Attrs SideScheme)
  deriving anyclass (IsSideScheme, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

-- | Boost: place 1 infamy counter on Criminal Enterprise, or, if you cannot, remove 1
-- madness counter from State of Madness.
instance RunMessage CollapsingBridge where
  runMessage msg s@(CollapsingBridge attrs) = runQueueT $ case msg of
    Boost (RevealedAsBoost target _) | isTarget attrs target -> s <$ infamyOrMadness 1
    _ -> CollapsingBridge <$> runMessage msg attrs
