module Marvel.Treachery.Treacheries.CrisisOfInfiniteDeadpools where

import Marvel.Prelude
import Marvel.Ability ()
import Marvel.Card
import Marvel.Game.Source
import Marvel.EncounterSet (EncounterSet (DreadpoolSet))
import Marvel.Card.Code ()
import Marvel.Entity
import Marvel.Message.Lifted
import Marvel.Modifier
import Marvel.Ref
import Marvel.Treachery.Cards qualified as Cards
import Marvel.Treachery.Types

crisisOfInfiniteDeadpools :: TreacheryCard CrisisOfInfiniteDeadpools
crisisOfInfiniteDeadpools = treachery CrisisOfInfiniteDeadpools Cards.crisisOfInfiniteDeadpools

newtype CrisisOfInfiniteDeadpools = CrisisOfInfiniteDeadpools (Attrs Treachery)
  deriving anyclass (IsTreachery, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

instance RunMessage CrisisOfInfiniteDeadpools where
  runMessage msg t@(CrisisOfInfiniteDeadpools attrs) = runQueueT $ case msg of
    TreacheryMessage tid (RevealTreachery you) | tid == treacheryId attrs -> do
      aside <- getSetAsideEncounterCards
      for_ [card | card <- aside, toCardCode card == "44038"] (revealEncounterCard you)
      cardSpecific attrs "revealDreadfulDeeds" you
      pure t
    CardSpecific (isTarget attrs -> True) "revealDreadfulDeeds" (fromJSON -> Success you) -> do
      aside <- getSetAsideEncounterCards
      for_ [card | card <- aside, toCardCode card == "44039"] (revealEncounterCard you)
      cardSpecific attrs "shuffleRemaining" ()
      pure t
    CardSpecific (isTarget attrs -> True) "shuffleRemaining" _ -> do
      aside <- filter ((== Just DreadpoolSet) . cdEncounterSet . getCardDef) <$> getSetAsideEncounterCards
      unless (null aside) $ shuffleIntoEncounterDeck aside
      removeFromGame attrs
      pure t
    _ -> CrisisOfInfiniteDeadpools <$> runMessage msg attrs
