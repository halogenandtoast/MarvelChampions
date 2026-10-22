module Marvel.Treachery.Treacheries.RaidTheArmory (
  raidTheArmory,
  RaidTheArmory (..),
) where

import Marvel.Prelude

import Marvel.Card
import Marvel.Entity
import Marvel.Message.Lifted
import Marvel.Ref
import Marvel.Trait
import Marvel.Treachery.Cards qualified as Cards
import Marvel.Treachery.Types

raidTheArmory :: TreacheryCard RaidTheArmory
raidTheArmory = treachery RaidTheArmory Cards.raidTheArmory

newtype RaidTheArmory = RaidTheArmory (Attrs Treachery)
  deriving anyclass (IsTreachery)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

-- | Incite 1 (its keyword). When Revealed: discard cards from the encounter deck until a
-- Weapon attachment is discarded, and reveal it.
instance RunMessage RaidTheArmory where
  runMessage msg t@(RaidTheArmory attrs) = runQueueT $ case msg of
    TreacheryMessage ident (RevealTreachery you) | ident == treacheryId attrs -> do
      discardUntil FromEncounterDeck (CardWithType AttachmentType <> CardWithTrait Weapon) attrs
      pure . RaidTheArmory $ attrs & resolverL ?~ you
    WithDiscardedMatch (isTarget attrs -> True) _ (EncounterCard card) -> do
      for_ (treacheryResolver attrs) \you -> revealEncounterCard you card
      pure t
    _ -> RaidTheArmory <$> runMessage msg attrs
