module Marvel.Ally.Allies.AngelaAldrifOdinsdottir (
  angelaAldrifOdinsdottir,
  AngelaAldrifOdinsdottir (..),
) where

import Marvel.Prelude

import Marvel.Ability
import Marvel.Ally.Cards qualified as Cards
import Marvel.Ally.Runner
import Marvel.Card.Def ()
import Marvel.Cost
import Marvel.Criteria
import Marvel.Window

angelaAldrifOdinsdottir :: AllyCard AngelaAldrifOdinsdottir
angelaAldrifOdinsdottir = ally AngelaAldrifOdinsdottir Cards.angelaAldrifOdinsdottir (Thw 0, 1) (Atk 2, 1) (HP 3)

newtype AngelaAldrifOdinsdottir = AngelaAldrifOdinsdottir (Attrs Ally)
  deriving anyclass (IsAlly, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

-- | "Forced Response: After Angela enters play under your control, ..."
instance HasAbilities AngelaAldrifOdinsdottir where
  getAbilities a =
    [limitedWindowAbility a 1 EntersPlayThis ForcedResponse OwnsThis NoCost $ runAbility a 1]

-- | Search the top 10 cards of the encounter deck for a minion and put it into play
-- engaged with you. Shuffle the encounter deck. If a minion was not put into play this
-- way, discard Angela.
instance RunMessage AngelaAldrifOdinsdottir where
  runMessage msg a@(AngelaAldrifOdinsdottir attrs) = runQueueT $ case msg of
    RanAbility _ (isTarget a -> True) 1 _ _ -> do
      lookAtTopOfEncounterDeck 10 a
      pure a
    SearchFoundCards (isTarget a -> True) (onlyEncounterCards -> cards) -> do
      let you = allyController attrs
      case filter ((== MinionType) . cdCardType . getCardDef) cards of
        [] -> do
          shuffleEncounterDeck
          removeFromPlay a
        minions ->
          lookingAt you (EncounterCard <$> cards) $
            chooseOne you
              [ targeting c do
                  putEncounterCardIntoPlay you c
                  shuffleEncounterDeck
              | c <- minions
              ]
      pure a
    _ -> AngelaAldrifOdinsdottir <$> runMessage msg attrs
