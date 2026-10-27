module Marvel.Treachery.Treacheries.HonorAmongThieves (
  honorAmongThieves,
  HonorAmongThieves (..),
) where

import Marvel.Prelude

import Marvel.Card
import Marvel.Entity
import Marvel.Id
import Marvel.Matchers
import Marvel.Message.Lifted
import Marvel.Query
import Marvel.Ref
import Marvel.Trait
import Marvel.Treachery.Cards qualified as Cards
import Marvel.Treachery.Types

honorAmongThieves :: TreacheryCard HonorAmongThieves
honorAmongThieves = treachery HonorAmongThieves Cards.honorAmongThieves

-- | When Revealed: discard cards from the top of the encounter deck until a Criminal
-- minion is discarded. Reveal that minion, then give it a tough status card and the
-- villain 1 facedown boost card.
newtype HonorAmongThieves = HonorAmongThieves (Attrs Treachery)
  deriving anyclass (IsTreachery)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

instance RunMessage HonorAmongThieves where
  runMessage msg t@(HonorAmongThieves attrs) = runQueueT $ case msg of
    TreacheryMessage ident (RevealTreachery you) | ident == treacheryId attrs -> do
      discardUntil FromEncounterDeck (CardWithType MinionType <> CardWithTrait Criminal) attrs
      pure . HonorAmongThieves $ attrs & resolverL ?~ you
    WithDiscardedMatch (isTarget attrs -> True) _ (EncounterCard card) -> do
      for_ (treacheryResolver attrs) \you -> do
        revealEncounterCard you card
        enemyBecomesTough (MinionId $ unCardId $ ecCardId card)
        selectOne ActiveVillain >>= traverse_ dealBoost
      pure t
    _ -> HonorAmongThieves <$> runMessage msg attrs
