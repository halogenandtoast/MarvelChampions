module Marvel.SideScheme.SideSchemes.TaskmastersTrainingCamp (
  taskmastersTrainingCamp,
  TaskmastersTrainingCamp (..),
) where

import Marvel.Prelude

import Marvel.Card.Code
import Marvel.Entity
import Marvel.GameValue
import Marvel.Message.Lifted
import Marvel.Modifier
import Marvel.SideScheme.Cards qualified as Cards
import Marvel.SideScheme.Types
import Marvel.Window qualified as W

taskmastersTrainingCamp :: SideSchemeCard TaskmastersTrainingCamp
taskmastersTrainingCamp = sideScheme TaskmastersTrainingCamp Cards.taskmastersTrainingCamp (PerPlayer 2)

newtype TaskmastersTrainingCamp = TaskmastersTrainingCamp (Attrs SideScheme)
  deriving anyclass (IsSideScheme, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode)

-- | "Forced Response: After a minion enters play, give it a tough status card."
instance RunMessage TaskmastersTrainingCamp where
  runMessage msg s@(TaskmastersTrainingCamp attrs) = runQueueT $ case msg of
    CheckWindows ws | minions@(_ : _) <- [mid | W.Window W.After (W.MinionEnteredPlay mid) <- ws] -> do
      for_ minions enemyBecomesTough
      pure s
    _ -> TaskmastersTrainingCamp <$> runMessage msg attrs
