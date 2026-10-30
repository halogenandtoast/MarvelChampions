module Marvel.Minion.Minions.SmSandman where

import Marvel.Prelude
import Marvel.Ability
import Marvel.Environment.Environments.SmSandman
import Marvel.Minion.Cards qualified as Cards
import Marvel.Minion.Runner

newtype SmSandmanMinion = SmSandmanMinion (Attrs Minion)
  deriving anyclass (IsMinion, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

smSandClone :: MinionCard SmSandmanMinion
smSandClone = minion SmSandmanMinion Cards.smSandClone (Sch 1) (Atk 0) (HP 3)

instance HasModifiersFor SmSandmanMinion where
  getModifiersFor _ target (SmSandmanMinion a)
    | target == toRef a = pure . pure . AttackModifier . fromIntegral =<< sandCounters
    | otherwise = pure []

instance RunMessage SmSandmanMinion where
  runMessage msg (SmSandmanMinion attrs) = runQueueT $ case msg of
    MinionMessage mid MinionDefeated | mid == minionId attrs -> do
      surgingSands
      SmSandmanMinion <$> runMessage msg attrs
    _ -> SmSandmanMinion <$> runMessage msg attrs
