module Marvel.Ally.Allies.MultipleMan (
  multipleMan,
  MultipleMan (..),
) where

import Marvel.Prelude

import Marvel.Ability
import Marvel.Ally.Cards qualified as Cards
import Marvel.Ally.Runner
import Marvel.Card.Def (CardMatcher (CardIs), cardMatch)
import Marvel.Cost
import Marvel.Criteria
import Marvel.Deck
import Marvel.Hand
import Marvel.Identity.Types (Field (..))
import Marvel.Projection
import Marvel.Window

multipleMan :: AllyCard MultipleMan
multipleMan = ally MultipleMan Cards.multipleMan (Thw 1, 1) (Atk 1, 1) (HP 2)

newtype MultipleMan = MultipleMan (Attrs Ally)
  deriving anyclass (IsAlly, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, IsRef)

-- | Played or put into play, so each copy put in answers again (the window is this
-- ally's own).
instance HasAbilities MultipleMan where
  getAbilities a =
    [ limitedWindowAbility a 1 EntersPlayThis Response OwnsThis NoCost $
        runAbility a 1
    ]

-- | Search the deck and hand for a copy and put it into play, then shuffle the deck,
-- which was searched whichever the copy came from.
instance RunMessage MultipleMan where
  runMessage msg a@(MultipleMan attrs) = runQueueT $ case msg of
    RanAbility ident (isTarget a -> True) 1 _ _ -> do
      hand <- projectMap PlayerIdentityHand unHand ident
      deck <- projectMap PlayerIdentityDeck unDeck ident
      let found = filter (cardMatch $ CardIs Cards.multipleMan) (hand <> deck)
      focusing (map PlayerCard found) $
        chooseOne ident $
          if null found
            then [labeled "$label.noMatchingCardsFound" (pure ())]
            else [targeting c $ putIntoPlay ident c | c <- found]
      shuffleDeck ident
      pure a
    _ -> MultipleMan <$> runMessage msg attrs
