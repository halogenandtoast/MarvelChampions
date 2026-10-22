module Marvel.Ally.Allies.Clea (
  clea,
  Clea (..),
) where

import Marvel.Prelude

import Marvel.Ability
import Marvel.Ally.Cards qualified as Cards
import Marvel.Ally.Runner
import Marvel.Card
import Marvel.Cost
import Marvel.Criteria
import Marvel.Matchers
import Marvel.Queue
import Marvel.Window qualified as W

clea :: AllyCard Clea
clea = ally Clea Cards.clea (Thw 1, 1) (Atk 1, 1) (HP 2)

newtype Clea = Clea (Attrs Ally)
  deriving anyclass (IsAlly, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, IsRef)

instance HasAbilities Clea where
  getAbilities (Clea a) =
    [ limitedWindowAbility a 1 (W.AllyDefeated W.When $ AllyWithId $ allyId a) Interrupt OwnsThis NoCost $
        runAbility a 1
    ]

-- | When she is defeated, she is shuffled into her owner's deck instead of staying in
-- their discard pile: she leaves play as she would, and goes on from there.
instance RunMessage Clea where
  runMessage msg a@(Clea attrs) = case msg of
    RanAbility _ (isTarget a -> True) 1 _ _ -> do
      let
        owner = allyController attrs
        cards = [c | PlayerCard c <- [toCard attrs]]
      replaceMatchingMessage
        (\m -> [m, IdentityMessage owner (ShuffleIntoIdentityDeck cards)])
        (== RemoveFromPlay (toTarget attrs))
      pure a
    _ -> Clea <$> runMessage msg attrs
