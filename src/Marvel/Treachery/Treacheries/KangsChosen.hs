module Marvel.Treachery.Treacheries.KangsChosen (
  kangsChosen,
  KangsChosen (..),
) where

import Marvel.Prelude

import Marvel.Card
import Marvel.Entity
import Marvel.Message.Lifted
import Marvel.Ref
import Marvel.Trait (Trait (Temporal))
import Marvel.Treachery.Cards qualified as Cards
import Marvel.Treachery.Types

kangsChosen :: TreacheryCard KangsChosen
kangsChosen = treachery KangsChosen Cards.kangsChosen

newtype KangsChosen = KangsChosen (Attrs Treachery)
  deriving anyclass (IsTreachery)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

-- | Incite 1. Discard from the encounter deck until a Temporal minion is discarded, and
-- reveal it.
instance RunMessage KangsChosen where
  runMessage msg t@(KangsChosen attrs) = runQueueT $ case msg of
    TreacheryMessage ident (RevealTreachery you) | ident == treacheryId attrs -> do
      discardUntil FromEncounterDeck (CardWithType MinionType <> CardWithTrait Temporal) attrs
      pure . KangsChosen $ attrs & resolverL ?~ you
    WithDiscardedMatch target FromEncounterDeck (EncounterCard minion) | isTarget attrs target -> do
      for_ (treacheryResolver attrs) \you -> revealEncounterCard you minion
      pure t
    _ -> KangsChosen <$> runMessage msg attrs
