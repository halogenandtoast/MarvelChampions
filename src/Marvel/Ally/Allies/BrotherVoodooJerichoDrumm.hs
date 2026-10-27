module Marvel.Ally.Allies.BrotherVoodooJerichoDrumm (
  brotherVoodooJerichoDrumm,
  BrotherVoodooJerichoDrumm (..),
) where

import Marvel.Prelude

import Marvel.Ability
import Marvel.Ally.Cards qualified as Cards
import Marvel.Ally.Runner
import Marvel.Cost
import Marvel.Criteria
import Marvel.Window

brotherVoodooJerichoDrumm :: AllyCard BrotherVoodooJerichoDrumm
brotherVoodooJerichoDrumm =
  ally BrotherVoodooJerichoDrumm Cards.brotherVoodooJerichoDrumm (Thw 2, 2) (Atk 1, 1) (HP 3)

newtype BrotherVoodooJerichoDrumm = BrotherVoodooJerichoDrumm (Attrs Ally)
  deriving anyclass (IsAlly, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, IsRef)

instance HasAbilities BrotherVoodooJerichoDrumm where
  getAbilities a =
    [limitedWindowAbility a 1 EntersPlayThis Response OwnsThis NoCost $ runAbility a 1]

-- | After he enters play, search the top 5 cards of your deck for an event card and add
-- it to your hand. Shuffle your deck.
instance RunMessage BrotherVoodooJerichoDrumm where
  runMessage msg a@(BrotherVoodooJerichoDrumm attrs) = runQueueT $ case msg of
    RanAbility ident (isTarget a -> True) 1 _ _ -> do
      search ident (SearchIdentityDeck ident $ TopOfDeck 5) (CardWithType EventType) SearchDrawOne ShuffleBackIn
      pure a
    _ -> BrotherVoodooJerichoDrumm <$> runMessage msg attrs
