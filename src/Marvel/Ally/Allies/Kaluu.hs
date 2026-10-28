module Marvel.Ally.Allies.Kaluu (
  kaluu,
  Kaluu (..),
) where

import Marvel.Prelude

import Marvel.Ability
import Marvel.Ally.Cards qualified as Cards
import Marvel.Ally.Runner
import Marvel.Cost
import Marvel.Criteria
import Marvel.Window

kaluu :: AllyCard Kaluu
kaluu = ally Kaluu Cards.kaluu (Thw 1, 1) (Atk 1, 1) (HP 2)

newtype Kaluu = Kaluu (Attrs Ally)
  deriving anyclass (IsAlly, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, IsRef)

instance HasAbilities Kaluu where
  getAbilities a =
    [limitedWindowAbility a 1 EntersPlayThis Response OwnsThis NoCost $ runAbility a 1]

-- | After he enters play, search the top 5 cards of your deck for an event and add it to
-- your hand. Shuffle your deck. (Brother Voodoo's text.)
instance RunMessage Kaluu where
  runMessage msg a@(Kaluu attrs) = runQueueT $ case msg of
    RanAbility ident (isTarget a -> True) 1 _ _ -> do
      search ident (SearchIdentityDeck ident $ TopOfDeck 5) (CardWithType EventType) SearchDrawOne ShuffleBackIn
      pure a
    _ -> Kaluu <$> runMessage msg attrs
