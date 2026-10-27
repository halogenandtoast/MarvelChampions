module Marvel.MainScheme.MainSchemes.TheGrandCollection where

import Marvel.Prelude

import Marvel.Ability
import Marvel.Card
import Marvel.Cost
import Marvel.Criteria
import Marvel.Entity
import Marvel.Game.Source
import Marvel.GameValue
import Marvel.MainScheme.Cards qualified as Cards
import Marvel.MainScheme.Types
import Marvel.Message.Lifted
import Marvel.Question (runAbility)
import Marvel.Ref

-- | 1B: threat 10 per player, 4 per player to begin with, acceleration 1 per player.
theGrandCollection :: MainSchemeCard TheGrandCollection
theGrandCollection = mainScheme TheGrandCollection Cards.theGrandCollection (PerPlayer 10) (PerPlayer 4) (PerPlayer 1)

newtype TheGrandCollection = TheGrandCollection (Attrs MainScheme)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode)

-- | 1B: "Hero Action: Choose to either exhaust your hero or spend 2 resources of any type
-- → discard 1 card from The Collection (to its owner's discard pile). (Limit once per
-- round per player.)" One ability with either cost: the two are equal abilities (the
-- same card and index), so the one limit holds for both.
instance IsMainScheme TheGrandCollection where
  mainSchemeAbilities (TheGrandCollection a) =
    [ limited (PerRound 1) $ ability a 1 HeroAction CollectionHasCards cost (runAbility a 1)
    | cost <- [ExhaustCost, MultiResourceCost [Nothing, Nothing]]
    ]

instance RunMessage TheGrandCollection where
  runMessage msg ms@(TheGrandCollection attrs) = runQueueT $ case msg of
    -- 1A Setup: "Create "The Collection" game area. Put the top card of each player's
    -- deck faceup into The Collection."
    SetupMainScheme -> do
      players <- getAllPlayers
      for_ players placeTopOfDeckInCollection
      pure ms
    RanAbility you (isTarget attrs -> True) 1 _ _ -> do
      cards <- getCollection
      chooseOne you [targeting (CardIdRef $ toCardId c) (discardFromCollection c) | c <- cards]
      pure ms
    -- "If there are at least 5[per_hero] cards in The Collection or if this stage is
    -- completed, the players lose the game."
    PlaceInCollection _ -> do
      cardSpecific attrs "collectionComplete" ()
      pure ms
    CardSpecific (isTarget attrs -> True) "collectionComplete" _ -> do
      n <- length <$> getCollection
      limit <- fromGameValue (PerPlayer 5)
      when (n >= limit) $ gameOver Lost
      pure ms
    AdvanceMainScheme ((== mainSchemeId attrs) -> True) -> ms <$ gameOver Lost
    _ -> TheGrandCollection <$> runMessage msg attrs
