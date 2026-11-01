module Marvel.Scenario.AgeOfApocalypseUnus (AoaUnusScenario (..), aoaUnusScenario) where

import Marvel.Prelude
import Marvel.EncounterSet qualified as Set
import Marvel.Message.Lifted
import Marvel.Scenario.Types

newtype AoaUnusScenario = AoaUnusScenario (Attrs Scenario)
  deriving anyclass IsScenario
  deriving newtype (Show, Eq, ToJSON, FromJSON)

aoaUnusScenario :: ScenarioCard AoaUnusScenario
aoaUnusScenario = scenario AoaUnusScenario "45059" ("45059", "45060") ["45062"] [Set.AoaUnus, Set.AoaInfinites]

instance RunMessage AoaUnusScenario where
  runMessage msg (AoaUnusScenario a) = AoaUnusScenario <$> runMessage msg a
