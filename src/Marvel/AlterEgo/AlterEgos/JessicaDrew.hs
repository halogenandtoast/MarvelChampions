module Marvel.AlterEgo.AlterEgos.JessicaDrew where

import Marvel.Prelude

import Marvel.Ability
import Marvel.AlterEgo.Cards qualified as Cards
import Marvel.AlterEgo.Runner
import Marvel.Area
import Marvel.Card
import Marvel.Cost.Types
import Marvel.Criteria
import Marvel.Deck
import Marvel.Entity
import Marvel.Game.Source
import Marvel.GameValue
import Marvel.Hand
import Marvel.Identity.Types (Field (..))
import Marvel.Message.Lifted
import Marvel.Modifier
import Marvel.Obligation.Cards qualified as Cards
import Marvel.Projection
import Marvel.Question
import Marvel.Ref
import Marvel.Stats

jessicaDrew :: AlterEgoCard JessicaDrew
jessicaDrew =
  alterEgo
    JessicaDrew
    Cards.jessicaDrew
    (HP $ Static 11)
    (HandSize 6)
    (Rec 3)
    [Cards.uncertainLoyalties]

newtype JessicaDrew = JessicaDrew (Attrs AlterEgo)
  deriving anyclass (IsAlterEgo, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, IsRef)

-- | Action: Look at the top card of any deck. (Limit once per round.) Her Double Agent
-- deckbuilding is checked with the deck ('aspectsBalanced').
instance HasAbilities JessicaDrew where
  getAbilities a =
    [ label "Double Agent" $
        limitedAbility a 1 (PerRound 1) Action IsSelf NoCost $
          runAbility a 1
    ]

-- | Each player's deck and each encounter deck with a card in it may be chosen; its top
-- card is shown to her player alone while she looks (RR "Look at").
instance RunMessage JessicaDrew where
  runMessage msg ae@(JessicaDrew a) = runQueueT $ case msg of
    RanAbility ident (isTarget a -> True) 1 _ _ -> do
      players <- getPlayers
      playerTops <- for players \p -> (p,) . take 1 <$> projectMap PlayerIdentityDeck unDeck p
      keys <- getDeckKeys
      encounterTops <- for keys \k -> (k,) . take 1 . pileDeck <$> getPile k
      let look c = lookingAt ident [c] $ chooseOne ident [labeled "$label.done" (pure ())]
      chooseOnePrompted ident "$label.chooseDeckToLookAt" $
        [targeting p (look $ PlayerCard c) | (p, [c]) <- playerTops]
          <> [labeled (deckLabel k) (look $ EncounterCard c) | (k, [c]) <- encounterTops]
      pure ae
    _ -> JessicaDrew <$> runMessage msg a
   where
    deckLabel = \case
      EncounterDeckKey -> "$label.encounterDeck"
      VillainDeckKey _ -> "$label.villainDeck"
      NamedDeckKey t -> t
      SideSchemeDeckKey -> "$label.sideSchemeDeck"
      InfinityStoneDeckKey -> "$label.infinityStoneDeck"
