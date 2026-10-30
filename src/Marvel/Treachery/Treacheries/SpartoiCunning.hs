module Marvel.Treachery.Treacheries.SpartoiCunning (
  spartoiCunning,
  SpartoiCunning (..),
) where

import Marvel.Prelude

import Marvel.Card.Code
import Marvel.Damage
import Marvel.Entity
import Marvel.Matchers
import Marvel.Message.Lifted
import Marvel.Ref
import Marvel.Treachery.Cards qualified as Cards
import Marvel.Treachery.Types

spartoiCunning :: TreacheryCard SpartoiCunning
spartoiCunning = treachery SpartoiCunning Cards.spartoiCunning

newtype SpartoiCunning = SpartoiCunning (Attrs Treachery)
  deriving anyclass (IsTreachery)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

-- | When Revealed: discard 1 card at random from your hand, take 1 damage, and place 1
-- threat on the main scheme.
instance RunMessage SpartoiCunning where
  runMessage msg t@(SpartoiCunning attrs) = runQueueT $ case msg of
    TreacheryMessage ident (RevealTreachery you) | ident == treacheryId attrs -> do
      discardFrom you RandomFromHand 1
      damageIdentity you attrs (toDamage 1 FromTreachery)
      placeThreatOnEach attrs 1 EncounterMainScheme
      pure t
    _ -> SpartoiCunning <$> runMessage msg attrs
