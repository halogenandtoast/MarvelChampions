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
  runMessage msg a@(Heimdall attrs) = case msg of
    RanAbility _ (isTarget a -> True) 1 _ _ -> do
      push $ LookAtTopOfEncounterDeck 3 (toTarget a)
      pure a
    SearchFoundCards (isTarget a -> True) (onlyEncounterCards -> cards) -> do
      pushAll
        [ FocusCards $ EncounterCard <$> cards
        , Ask (allyController attrs) $
            promptedAbout "$label.chooseCardToDiscard" a $
              ChooseOne
                [ TargetLabel (toRef c) [Run [DiscardedCard (EncounterCard c), order [] (delete c cards)]]
                | c <- cards
                ]
        , UnfocusCards
        ]
      pure a
    CardSpecific (isTarget a -> True) "order" (decode -> Just (chosen, rest))
      | length rest <= 1 -> do
          push $ PutOnTopOfEncounterDeck (chosen <> rest)
          pure a
      | otherwise -> do
          push $
            Ask (allyController attrs) $
              promptedAbout "$label.chooseNextCardOnTop" a $
                ChooseOne [TargetLabel (toRef c) [Run [order (chosen <> [c]) (delete c rest)]] | c <- rest]
          pure a
    _ -> Heimdall <$> runMessage msg attrs
   where
    order :: [EncounterCard] -> [EncounterCard] -> Message
    order chosen rest = cardSpecific a "order" (chosen, rest)
    decode v = case fromJSON v of
      Success x -> Just x
      Error _ -> Nothing
