module Marvel.Minion.Minions.Taskmaster (
  taskmaster,
  Taskmaster (..),
) where

import Marvel.Prelude

import Marvel.Ability
import Marvel.Matchers
import Marvel.Minion.Cards qualified as Cards
import Marvel.Minion.Runner

taskmaster :: MinionCard Taskmaster
taskmaster = minion Taskmaster Cards.taskmaster (Sch 0) (Atk 0) (HP 4)

newtype Taskmaster = Taskmaster (Attrs Minion)
  deriving anyclass (IsMinion, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode)

-- | +1 SCH and +1 ATK for each upgrade the player he is engaged with controls.
instance HasModifiersFor Taskmaster where
  getModifiersFor _ (MinionRef mid) (Taskmaster attrs) | mid == minionId attrs = do
    n <- selectListCount (UpgradeControlledBy $ IdentityWithId $ minionEngagedIdentity attrs)
    pure [SchemeModifier n, AttackModifier n]
  getModifiersFor _ _ _ = pure []

-- | As a boost card: the villain gets as much for the activation, counting the upgrades
-- of the player it is against.
instance RunMessage Taskmaster where
  runMessage msg e@(Taskmaster attrs) = case msg of
    Boost (RevealedAsBoost target (EnemyVillainId vid)) | isTarget attrs target -> do
      n <- selectListCount (UpgradeControlledBy You)
      push $ VillainMessage vid (VillainAddBoost n)
      pure e
    _ -> Taskmaster <$> runMessage msg attrs
