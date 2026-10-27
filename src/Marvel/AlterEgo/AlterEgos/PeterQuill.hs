module Marvel.AlterEgo.AlterEgos.PeterQuill (
  peterQuill,
  PeterQuill (..),
) where

import Marvel.Prelude

import Data.List (find)

import Marvel.Ability
import Marvel.AlterEgo.Cards qualified as Cards
import Marvel.AlterEgo.Runner
import Marvel.Card.Def
import Marvel.Cost.Types
import Marvel.Criteria
import Marvel.Deck
import Marvel.Discard
import Marvel.Entity
import Marvel.GameValue
import Marvel.Hand
import Marvel.Identity.Types
import Marvel.Matchers
import Marvel.Message.Lifted
import Marvel.Modifier
import Marvel.Obligation.Cards qualified as Cards
import Marvel.Projection
import Marvel.Question
import Marvel.Ref
import Marvel.Stats
import Marvel.Upgrade.Cards qualified as Cards

peterQuill :: AlterEgoCard PeterQuill
peterQuill =
  alterEgo
    PeterQuill
    Cards.peterQuill
    (HP $ Static 10)
    (HandSize 6)
    (Rec 3)
    [Cards.banishment]

newtype PeterQuill = PeterQuill (Attrs AlterEgo)
  deriving anyclass (IsAlterEgo, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, IsRef)

-- | Setup: search your deck and discard pile for an Element Gun and add it to your hand.
-- Smooth Talker — Action: swap a card in your hand with the top card of your deck.
-- (Limit once per round.)
instance HasAbilities PeterQuill where
  getAbilities a@(PeterQuill attrs) =
    [ ability a 1 Setup NoCriteria NoCost $ runAbility a 1
    , label "Smooth Talker" $
        limitedAbility a 2 (PerRound 1) Action (IsSelf <> hasCardIn InHandOf) NoCost $
          runAbility a 2
    ]
   where
    -- a card to swap (the deck is never empty for long: it is reshuffled at once)
    hasCardIn zone = ExtendedCardExists $ zone (IdentityWithId $ alterEgoIdentityId attrs) (BasicCardMatches AnyCard)

instance RunMessage PeterQuill where
  runMessage msg ae@(PeterQuill a) = runQueueT $ case msg of
    RanAbility ident (isTarget a -> True) 1 _ _ -> do
      deck <- project PlayerIdentityDeck ident
      discard <- project PlayerIdentityDiscard ident
      for_ (find (sameCard Cards.elementGun . getCardDef) (unDeck deck <> unDiscard discard)) $
        addToHand ident
      shuffleDeck ident
      pure ae
    RanAbility ident (isTarget a -> True) 2 _ _ -> do
      hand <- projectMap PlayerIdentityHand unHand ident
      top <- projectMap PlayerIdentityDeck (headMay . unDeck) ident
      -- with no top card there is nothing to swap with
      for_ top \t ->
        chooseOnePrompted ident "$label.chooseCardToSwap"
          [targeting c (addToHand ident t >> placeOnTopOfDeck ident c) | c <- hand]
      pure ae
    _ -> PeterQuill <$> runMessage msg a
