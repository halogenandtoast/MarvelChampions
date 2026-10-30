module Marvel.Treachery.Treacheries.TechnologicalEnhancements (
  technologicalEnhancements,
  TechnologicalEnhancements (..),
) where

import Marvel.Prelude

import Marvel.Card.Code
import Marvel.Entity
import Marvel.Id
import Marvel.Matchers
import Marvel.Message.Lifted
import Marvel.Query
import Marvel.Treachery.Cards qualified as Cards
import Marvel.Treachery.Types

technologicalEnhancements :: TreacheryCard TechnologicalEnhancements
technologicalEnhancements = treachery TechnologicalEnhancements Cards.technologicalEnhancements

newtype TechnologicalEnhancements = TechnologicalEnhancements (Attrs Treachery)
  deriving anyclass (IsTreachery)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode)

-- | Incite 1 is on the card. When Revealed, and [star] Boost: 1 test counter on the
-- main scheme.
instance RunMessage TechnologicalEnhancements where
  runMessage msg t@(TechnologicalEnhancements attrs) = runQueueT $ case msg of
    TreacheryMessage ident (RevealTreachery _) | treacheryId attrs == ident -> do
      testCounter
      pure t
    Boost (RevealedAsBoost target _) | isTarget attrs target -> do
      testCounter
      pure t
    _ -> TechnologicalEnhancements <$> runMessage msg attrs
   where
    testCounter = selectOne EncounterMainScheme >>= traverse_ \case
      SchemeMainSchemeId ms -> placeCounters ms 1
      _ -> pure ()
