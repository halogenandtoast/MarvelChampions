module Marvel.Minion.Minions.AncientWarrior (
  ancientWarrior,
  AncientWarrior (..),
) where

import Marvel.Prelude

import Marvel.Ability
import Marvel.Game.Source
import Marvel.Minion.Cards qualified as Cards
import Marvel.Minion.Runner

ancientWarrior :: MinionCard AncientWarrior
ancientWarrior = minion AncientWarrior Cards.ancientWarrior (Sch 1) (Atk 2) (HP 2)

newtype AncientWarrior = AncientWarrior (Attrs Minion)
  deriving anyclass (IsMinion, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

-- | Quickstrike. [star] Boost: you are stunned.
instance RunMessage AncientWarrior where
  runMessage msg e@(AncientWarrior attrs) = runQueueT $ case msg of
    Boost (RevealedAsBoost target enemy) | isTarget attrs target -> do
      you <- getBoostedPlayer enemy
      stunIdentity you
      pure e
    _ -> AncientWarrior <$> runMessage msg attrs
