module Marvel.Ally.Allies.Blindfold where

import Marvel.Prelude
import Data.List (delete)
import Marvel.Ability
import Marvel.Ally.Cards qualified as Cards
import Marvel.Ally.Runner
import Marvel.Card ()
import Marvel.Entity ()
import Marvel.Modifier ()
import Marvel.Ref ()
import Marvel.Stats ()
import Marvel.Cost
import Marvel.Criteria
import Marvel.Window qualified as W

blindfold :: AllyCard Blindfold
-- The printed ATK field has no consequential-damage icon (MarvelCDB says one).
blindfold = ally Blindfold Cards.blindfold (Thw 2, 1) (Atk 0, 0) (HP 2)

newtype Blindfold = Blindfold (Attrs Ally)
  deriving anyclass (IsAlly, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

instance HasAbilities Blindfold where
  getAbilities a = [limitedWindowAbility a 1 W.EntersPlayThis Response OwnsThis NoCost (runAbility a 1)]

instance RunMessage Blindfold where
  runMessage msg a@(Blindfold attrs) = runQueueT $ case msg of
    RanAbility _ (isTarget a -> True) 1 _ _ -> do
      lookAtTopOfEncounterDeck 5 a
      pure a
    SearchFoundCards (isTarget a -> True) (onlyEncounterCards -> cards) -> do
      unless (null cards) $ lookingAt (allyController attrs) (map EncounterCard cards) $
        chooseOnePromptedAbout (allyController attrs) "$label.chooseCardToDiscard" a
          [ targeting card do
              discardCard card
              putOnTopOfEncounterDeck (delete card cards)
          | card <- cards
          ]
      pure a
    _ -> Blindfold <$> runMessage msg attrs
