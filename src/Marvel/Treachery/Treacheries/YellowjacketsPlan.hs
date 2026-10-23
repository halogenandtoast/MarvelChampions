module Marvel.Treachery.Treacheries.YellowjacketsPlan (
  yellowjacketsPlan,
  YellowjacketsPlan (..),
) where

import Marvel.Prelude

import Marvel.Card
import Marvel.EncounterSet
import Marvel.Entity
import Marvel.Message.Lifted
import Marvel.Ref
import Marvel.Treachery.Cards qualified as Cards
import Marvel.Treachery.Types

yellowjacketsPlan :: TreacheryCard YellowjacketsPlan
yellowjacketsPlan = treachery YellowjacketsPlan Cards.yellowjacketsPlan

newtype YellowjacketsPlan = YellowjacketsPlan (Attrs Treachery)
  deriving anyclass (IsTreachery)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

-- | When Revealed: Discard cards from the encounter deck until a card from the Ant-Man
-- Nemesis set is discarded this way. Reveal that card.
instance RunMessage YellowjacketsPlan where
  runMessage msg t@(YellowjacketsPlan attrs) = runQueueT $ case msg of
    TreacheryMessage ident (RevealTreachery you) | ident == treacheryId attrs -> do
      discardUntil FromEncounterDeck (CardInEncounterSet AntManNemesis) attrs
      pure . YellowjacketsPlan $ attrs & resolverL ?~ you
    WithDiscardedMatch target FromEncounterDeck (EncounterCard card) | isTarget attrs target -> do
      for_ (treacheryResolver attrs) \you -> revealEncounterCard you card
      pure t
    _ -> YellowjacketsPlan <$> runMessage msg attrs
