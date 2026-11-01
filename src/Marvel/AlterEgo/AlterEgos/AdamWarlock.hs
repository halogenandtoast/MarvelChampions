module Marvel.AlterEgo.AlterEgos.AdamWarlock (
  adamWarlockAlterEgo,
  AdamWarlockAlterEgo (..),
) where

import Marvel.Prelude

import Marvel.Ability
import Marvel.AlterEgo.Cards qualified as Cards
import Marvel.AlterEgo.Runner
import Marvel.Cost.Types
import Marvel.Criteria
import Marvel.Entity
import Marvel.GameValue
import Marvel.Hand
import Marvel.Matchers
import Marvel.Message.Lifted
import Marvel.Modifier
import Marvel.Obligation.Cards qualified as Cards
import Marvel.Query
import Marvel.Question
import Marvel.Ref
import Marvel.Stats
import Marvel.Status ()

adamWarlockAlterEgo :: AlterEgoCard AdamWarlockAlterEgo
adamWarlockAlterEgo =
  alterEgo
    AdamWarlockAlterEgo
    Cards.adamWarlockAlterEgo
    (HP $ Static 11)
    (HandSize 6)
    (Rec 3)
    [Cards.regenerationCycle]

newtype AdamWarlockAlterEgo = AdamWarlockAlterEgo (Attrs AlterEgo)
  deriving anyclass (IsAlterEgo, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, IsRef)

-- | Avatar of Life is deck-building only ('Marvel.Decklist.deckBuildingError'). Action:
-- Discard a card from your hand → remove a status card from Adam Warlock.
instance HasAbilities AdamWarlockAlterEgo where
  getAbilities a@(AdamWarlockAlterEgo attrs) =
    [ ability a 1 Action (IsSelf <> AnyCriteria [CharacterExists (IdentityCharacters you <> CharacterWithStatus s) | s <- [minBound .. maxBound]]) (DiscardHandCardsCost 1 (Just 1)) (runAbility a 1)
    ]
   where
    you = IdentityWithId $ alterEgoIdentityId attrs

instance RunMessage AdamWarlockAlterEgo where
  runMessage msg ae@(AdamWarlockAlterEgo attrs) = runQueueT $ case msg of
    RanAbility ident (isTarget attrs -> True) 1 _ _ -> do
      held <- filterM (\s -> selectAny (IdentityCharacters (IdentityWithId ident) <> CharacterWithStatus s)) [minBound .. maxBound]
      chooseOnePrompted ident "$label.chooseStatusToDiscard" [labeled (statusLabel s) (removeStatus ident s) | s <- held]
      pure ae
    _ -> AdamWarlockAlterEgo <$> runMessage msg attrs
