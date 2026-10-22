module Marvel.Ally.Allies.Heimdall (
  heimdall,
  Heimdall (..),
) where

import Marvel.Prelude

import Data.List (delete)
import Marvel.Ability
import Marvel.Ally.Cards qualified as Cards
import Marvel.Ally.Runner
import Marvel.Cost
import Marvel.Criteria
import Marvel.Window

heimdall :: AllyCard Heimdall
heimdall = ally Heimdall Cards.heimdall (Thw 2, 1) (Atk 3, 2) (HP 4)

newtype Heimdall = Heimdall (Attrs Ally)
  deriving anyclass (IsAlly, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

instance HasAbilities Heimdall where
  getAbilities a =
    [limitedWindowAbility a 1 (PlayThis After) Response OwnsThis NoCost $ runAbility a 1]

-- | Look at the top 3 cards of the encounter deck, discard 1 of them, and put the
-- others back in the order chosen, the first chosen on top.
instance RunMessage Heimdall where
  runMessage msg a@(Heimdall attrs) = runQueueT $ case msg of
    RanAbility _ (isTarget a -> True) 1 _ _ -> do
      lookAtTopOfEncounterDeck 3 a
      pure a
    SearchFoundCards (isTarget a -> True) (onlyEncounterCards -> cards) -> do
      focusing (EncounterCard <$> cards) $
        chooseOnePromptedAbout (allyController attrs) "$label.chooseCardToDiscard" a
          [ targeting c do
              discardCard c
              order [] (delete c cards)
          | c <- cards
          ]
      pure a
    CardSpecific (isTarget a -> True) "order" (decode -> Just (chosen, rest))
      | length rest <= 1 -> do
          putOnTopOfEncounterDeck (chosen <> rest)
          pure a
      | otherwise -> do
          chooseOnePromptedAbout (allyController attrs) "$label.chooseNextCardOnTop" a
            [targeting c $ order (chosen <> [c]) (delete c rest) | c <- rest]
          pure a
    _ -> Heimdall <$> runMessage msg attrs
   where
    order :: (ReverseQueue m) => [EncounterCard] -> [EncounterCard] -> m ()
    order chosen rest = cardSpecific a "order" (chosen, rest)
    decode v = case fromJSON v of
      Success x -> Just x
      Error _ -> Nothing
