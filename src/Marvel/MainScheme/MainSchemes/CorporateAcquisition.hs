module Marvel.MainScheme.MainSchemes.CorporateAcquisition where

import Marvel.Prelude

import Marvel.Card.Code
import Marvel.Entity
import Marvel.GameValue
import Marvel.MainScheme.Cards qualified as Cards
import Marvel.MainScheme.Types
import Marvel.Message.Lifted

corporateAcquisition :: MainSchemeCard CorporateAcquisition
corporateAcquisition =
  mainScheme CorporateAcquisition Cards.corporateAcquisition (PerPlayer 10) (PerPlayer 1) (PerPlayer 2)

newtype CorporateAcquisition = CorporateAcquisition (Attrs MainScheme)
  deriving anyclass (IsMainScheme)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode)

instance RunMessage CorporateAcquisition where
  runMessage msg ms@(CorporateAcquisition attrs) = runQueueT $ case msg of
    AdvanceMainScheme ((== mainSchemeId attrs) -> True) -> ms <$ gameOver Lost
    _ -> CorporateAcquisition <$> runMessage msg attrs
