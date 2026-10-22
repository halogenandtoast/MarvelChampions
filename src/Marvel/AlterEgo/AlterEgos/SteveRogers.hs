module Marvel.AlterEgo.AlterEgos.SteveRogers where

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
import Marvel.Game.Source
import Marvel.GameValue
import Marvel.Hand
import Marvel.Identity.Types
import Marvel.Message.Lifted
import Marvel.Modifier
import Marvel.Obligation.Cards qualified as Cards
import Marvel.Projection
import Marvel.Question
import Marvel.Ref
import Marvel.Stats
import Marvel.Upgrade.Cards qualified as Cards

steveRogers :: AlterEgoCard SteveRogers
steveRogers =
  alterEgo
    SteveRogers
    Cards.steveRogers
    (HP $ Static 11)
    (HandSize 6)
    (Rec 3)
    [Cards.manOutOfTime]

newtype SteveRogers = SteveRogers (Attrs AlterEgo)
  deriving anyclass (IsAlterEgo)
  deriving newtype (Show, Eq, ToJSON, FromJSON, IsRef)

-- | Living Legend: the first ally the player plays each round costs 1 less, if no ally
-- has been played yet, in either form.
instance HasModifiersFor SteveRogers where
  getModifiersFor _ (IdentityRef ident) (SteveRogers a) | ident == alterEgoIdentityId a = do
    played <- getPlayedThisRound
    pure
      [ ResourceCostReductionFor (CardWithType AllyType) 1
      | not (any (\(p, def) -> p == ident && cdCardType def == AllyType) played)
      ]
  getModifiersFor _ _ _ = pure []

instance HasAbilities SteveRogers where
  getAbilities a = [ability a 1 Setup NoCriteria NoCost $ runAbility a 1]

instance RunMessage SteveRogers where
  runMessage msg ae@(SteveRogers a) = runQueueT $ case msg of
    -- the shield is searched for in the deck and the discard pile, then the deck is
    -- shuffled
    RanAbility ident (isTarget a -> True) 1 _ _ -> do
      deck <- project PlayerIdentityDeck ident
      discard <- project PlayerIdentityDiscard ident
      for_ (find (sameCard Cards.captainAmericasShield . getCardDef) (unDeck deck <> unDiscard discard)) $
        addToHand ident
      shuffleDeck ident
      pure ae
    _ -> SteveRogers <$> runMessage msg a
