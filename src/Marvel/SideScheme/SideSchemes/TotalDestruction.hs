module Marvel.SideScheme.SideSchemes.TotalDestruction (
  totalDestruction,
  TotalDestruction (..),
) where

import Marvel.Prelude

import Marvel.Card.Code
import Marvel.Entity
import Marvel.GameValue
import Marvel.Matchers
import Marvel.Message.Lifted
import Marvel.Minion.Cards qualified as Cards
import Marvel.Modifier
import Marvel.Query
import Marvel.Ref
import Marvel.SideScheme.Cards qualified as Cards
import Marvel.SideScheme.Types

totalDestruction :: SideSchemeCard TotalDestruction
totalDestruction = sideScheme TotalDestruction Cards.totalDestruction (PerPlayer 2)

newtype TotalDestruction = TotalDestruction (Attrs SideScheme)
  deriving anyclass (IsSideScheme)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode)

-- | "Threat cannot be removed from this scheme while Abomination is in play."
instance HasModifiersFor TotalDestruction where
  getModifiersFor _ (SideSchemeRef sid) (TotalDestruction a)
    | sid == sideSchemeId a = do
        abomination <- selectAny (MinionIs Cards.abomination)
        pure [ThreatCannotBeRemoved | abomination]
  getModifiersFor _ _ _ = pure []

instance RunMessage TotalDestruction where
  runMessage msg (TotalDestruction attrs) = runQueueT $ TotalDestruction <$> runMessage msg attrs
