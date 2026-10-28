-- | Odin, All-Father (The Mad Titan's Shadow, Hela): the scenario's player card, an ally
-- of two sides. Hela's set attaches him to Odin's Torment, Captive side faceup, until
-- Hall of Nastrond is defeated; the campaign puts him into play on his King side.
module Marvel.Ally.Allies.Odin (
  odinCaptive,
  odinKing,
  Odin (..),
) where

import Marvel.Prelude

import Marvel.Ability
import Marvel.Ally.Cards qualified as Cards
import Marvel.Ally.Runner
import Marvel.Trait (Trait (King))

odinCaptive :: AllyCard Odin
odinCaptive = ally Odin Cards.odinCaptive (Thw 2, 2) (Atk 3, 2) (HP 6)

odinKing :: AllyCard Odin
odinKing = ally Odin Cards.odinKing (Thw 3, 2) (Atk 4, 2) (HP 6)

newtype Odin = Odin (Attrs Ally)
  deriving anyclass (IsAlly, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, IsRef)

isKing :: Attrs Ally -> Bool
isKing a = King `member` cdTraits (allyCardDef a)

-- | In play he is not attached to the main scheme, so the Captive side's gained text
-- applies: the first player gains control of Odin; he cannot have cards attached (King:
-- encounter cards) and does not count against the ally limit. King: Forced Interrupt:
-- when Odin leaves play, remove him from the game (he never reaches a discard pile).
instance HasModifiersFor Odin where
  getModifiersFor _ (AllyRef aid) (Odin a)
    | aid == allyId a =
        pure $
          [NotCountedAgainstAllyLimit, ControlledByFirstPlayer]
            <> if isKing a
              then [CannotHaveAttachedEncounterCards, RemovedFromGameInsteadOfLeavingPlay]
              else [CannotHaveAttachedCards]
  getModifiersFor _ _ _ = pure []

-- | Captive: if Odin leaves play (defeated, discarded, returned to hand or removed from
-- the game), the players lose the game.
instance RunMessage Odin where
  runMessage msg (Odin attrs) = runQueueT $ case msg of
    RemoveFromPlay (AllyRef aid) | aid == allyId attrs, not (isKing attrs) -> leaves
    ReturnToHand (AllyRef aid) | aid == allyId attrs, not (isKing attrs) -> leaves
    RemoveFromGame (AllyRef aid) | aid == allyId attrs, not (isKing attrs) -> leaves
    _ -> Odin <$> runMessage msg attrs
   where
    leaves = do
      gameOver Lost
      Odin <$> runMessage msg attrs
