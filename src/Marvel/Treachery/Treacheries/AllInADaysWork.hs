module Marvel.Treachery.Treacheries.AllInADaysWork (
  allInADaysWork,
  AllInADaysWork (..),
) where

import Marvel.Prelude

import Marvel.Card.Code
import Marvel.Entity
import Marvel.Message.Lifted
import Marvel.Ref
import Marvel.Treachery.Cards qualified as Cards
import Marvel.Treachery.Types
import Marvel.Environment.Environments.CriminalEnterprise (infamyOrMadness)

allInADaysWork :: TreacheryCard AllInADaysWork
allInADaysWork = treachery AllInADaysWork Cards.allInADaysWork

newtype AllInADaysWork = AllInADaysWork (Attrs Treachery)
  deriving anyclass (IsTreachery)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

-- | When Revealed: 2 infamy counters, or, if you cannot, 2 madness counters removed;
-- Boost: 1 of either.
instance RunMessage AllInADaysWork where
  runMessage msg t@(AllInADaysWork attrs) = runQueueT $ case msg of
    TreacheryMessage ident (RevealTreachery _) | ident == treacheryId attrs -> t <$ infamyOrMadness 2
    Boost (RevealedAsBoost target _) | isTarget attrs target -> t <$ infamyOrMadness 1
    _ -> AllInADaysWork <$> runMessage msg attrs
