module Marvel.Treachery.Treacheries.SpreadingLies (
  spreadingLies,
  SpreadingLies (..),
) where

import Marvel.Prelude

import Marvel.Card.Code
import Marvel.Entity
import Marvel.Matchers
import Marvel.Message.Lifted
import Marvel.Query
import Marvel.Ref
import Marvel.Treachery.Cards qualified as Cards
import Marvel.Treachery.Types

spreadingLies :: TreacheryCard SpreadingLies
spreadingLies = treachery SpreadingLies Cards.spreadingLies

newtype SpreadingLies = SpreadingLies (Attrs Treachery)
  deriving anyclass (IsTreachery)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

-- | When Revealed: place 2 threat on each scheme in play. Boost: give Red Skull a tough
-- status card.
instance RunMessage SpreadingLies where
  runMessage msg t@(SpreadingLies attrs) = runQueueT $ case msg of
    TreacheryMessage ident (RevealTreachery _) | ident == treacheryId attrs -> do
      placeThreatOnEach attrs 2 AnyScheme
      pure t
    Boost (RevealedAsBoost target _) | isTarget attrs target -> do
      enemyBecomesTough =<< selectJust ActiveVillain
      pure t
    _ -> SpreadingLies <$> runMessage msg attrs
