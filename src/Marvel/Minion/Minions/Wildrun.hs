module Marvel.Minion.Minions.Wildrun (
  wildrun,
  Wildrun (..),
) where

import Marvel.Prelude

import Marvel.Ability
import Marvel.Game.Source
import Marvel.Minion.Cards qualified as Cards
import Marvel.Minion.Runner

wildrun :: MinionCard Wildrun
wildrun = minion Wildrun Cards.wildrun (Sch 1) (Atk 2) (HP 5)

newtype Wildrun = Wildrun (Attrs Minion)
  deriving anyclass (IsMinion, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

instance RunMessage Wildrun where
  runMessage msg e@(Wildrun attrs) = runQueueT $ case msg of
    -- When Revealed: discard 1 random card from your hand
    MinionMessage ident (RevealMinion you) | ident == minionId attrs -> do
      discardFrom you RandomFromHand 1
      pure e
    -- [star] Boost: the same, and this enemy is given another boost card
    Boost (RevealedAsBoost target enemy) | isTarget attrs target -> do
      you <- getBoostedPlayer enemy
      discardFrom you RandomFromHand 1
      dealBoost enemy
      pure e
    _ -> Wildrun <$> runMessage msg attrs
