-- | Collector, Infiltrate the Museum's villain (The Galaxy's Most Wanted): I and II, or II
-- and III in expert mode.
module Marvel.Villain.Villains.Collector where

import Marvel.Prelude

import Marvel.Ability
import Marvel.Card
import Marvel.Cost
import Marvel.Damage
import Marvel.Deck
import Marvel.Difficulty
import Marvel.Entity
import Marvel.Game.Source
import Marvel.GameValue
import Marvel.Hp
import Marvel.Identity.Types (Field (PlayerIdentityDeck))
import Marvel.Matchers
import Marvel.Message.Lifted
import Marvel.Modifier
import Marvel.Projection
import Marvel.Query
import Marvel.Question (runAbility)
import Marvel.Ref ()
import Marvel.Stats
import Marvel.Villain.Cards qualified as Cards
import Marvel.Villain.Types
import Marvel.Window
import Marvel.Window qualified as W

newtype Collector = Collector (Attrs Villain)
  deriving anyclass (IsVillain, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON)

collector :: VillainCard Collector
collector = collector1

collector1 :: VillainCard Collector
collector1 = villain Collector Cards.collector1 (Sch 2) (Atk 1) (HP $ PerPlayer 13)

collector2 :: VillainCard Collector
collector2 = villainWith Collector Cards.collector2 (Sch 3) (Atk 2) (HP $ PerPlayer 14) (stageL .~ 2)

collector3 :: VillainCard Collector
collector3 = villainWith Collector Cards.collector3 (Sch 4) (Atk 3) (HP $ PerPlayer 18) (stageL .~ 3)

-- | Every stage: "Forced Interrupt: When a card (player or encounter) would be placed into
-- a discard pile from play, put it faceup into The Collection instead" (III: "then place
-- 1 threat on the main scheme"). Only a card leaving play for a discard pile, not one
-- set aside, removed from the game, shuffled into a deck or returned to hand, nor one
-- discarded from a hand or a deck (TGMW FAQ p.21). II and III have a "When Revealed".
instance HasAbilities Collector where
  getAbilities (Collector a) =
    windowAbility a 1 CardWouldBeDiscardedFromPlay ForcedInterrupt NoCost (runAbility a 1)
      : [ windowAbility a 2 (VillainRevealed When (VillainWithId $ villainId a) RevealedFromVillain) WhenRevealed NoCost (runAbility a 2)
        | villainStage a >= 2
        ]

instance RunMessage Collector where
  runMessage msg e@(Collector attrs) = runQueueT $ case msg of
    VillainMessage ident VillainDefeated | ident == villainId attrs -> do
      difficulty <- getDifficulty
      case (difficulty, villainStage attrs) of
        (_, 1) -> do
          advanceVillain ident
          pure $ advanceVillainTo collector2 attrs
        (Expert, 2) -> do
          advanceVillain ident
          pure $ advanceVillainTo collector3 attrs
        _ -> Collector <$> runMessage msg attrs
    RanAbility _ (isTarget attrs -> True) 1 windows _ -> do
      for_ [either PlayerCard EncounterCard c | W.WouldBeDiscardedFromPlay c <- windows] \card ->
        insteadOf (== DiscardedCard card) do
          placeInCollection card
          when (villainStage attrs >= 3) $ selectOne EncounterMainScheme >>= traverse_ (`placeThreat` 1)
      pure e
    RanAbility _ (isTarget attrs -> True) 2 _ _ -> do
      players <- getAllPlayers
      case villainStage attrs of
        -- II: "In player order, each player must choose to either put the top card of
        -- their deck faceup into The Collection or take 3 damage."
        2 -> for_ players \p -> do
          cards <- projectP PlayerIdentityDeck (notNull . unDeck) p
          chooseOrRunOne p $
            [labeled "$label.topCardIntoTheCollection" (placeTopOfDeckInCollection p) | cards]
              <> [labeled (countLabel "takeDamage" 3) (damageIdentity p attrs (toDamage 3 FromAbility))]
        -- III: "Put the top card of each player's deck faceup into The Collection. Place
        -- 1 threat on the main scheme for each card in The Collection."
        _ -> do
          for_ players placeTopOfDeckInCollection
          cardSpecific attrs "threatForTheCollection" ()
      pure e
    CardSpecific (isTarget attrs -> True) "threatForTheCollection" _ -> do
      n <- fromIntegral . length <$> getCollection
      when (n > 0) $ selectOne EncounterMainScheme >>= traverse_ (`placeThreat` n)
      pure e
    _ -> Collector <$> runMessage msg attrs
