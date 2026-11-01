module Marvel.Scenario.NextEvolution where

import Marvel.Prelude
import Marvel.NextEvolution.SinisterScenario (prepareMisterSinisterScenario)
import Marvel.Message.Lifted
import Marvel.Scenario.NextEvolutionMarauders (runMaraudersScenarioMessage)
import Marvel.Scenario.Types
import Marvel.EncounterSet qualified as Set

newtype NextEvolutionScenario = NextEvolutionScenario (Attrs Scenario)
  deriving anyclass IsScenario
  deriving newtype (Show, Eq, ToJSON, FromJSON)

neMorlockSiegeScenario :: ScenarioCard NextEvolutionScenario
neMorlockSiegeScenario = scenario NextEvolutionScenario "40077" ("40070a", "40070b") ["40077", "40078"] [Set.MorlockSiege]

neOnTheRunScenario :: ScenarioCard NextEvolutionScenario
neOnTheRunScenario = scenario NextEvolutionScenario "40103" ("40070a", "40070b") ["40103", "40104"] [Set.OnTheRun, Set.MutantSlayers]

neJuggernautScenario :: ScenarioCard NextEvolutionScenario
neJuggernautScenario = scenario NextEvolutionScenario "40118" ("40118", "40119") ["40121"] [Set.NeJuggernaut, Set.HopeSummersSet]

neMisterSinisterScenario :: ScenarioCard NextEvolutionScenario
neMisterSinisterScenario = scenario NextEvolutionScenario "40136" ("40136", "40137") ["40139", "40140", "40141", "40142", "40143"] [Set.MisterSinister, Set.FlightSet, Set.SuperStrengthSet, Set.TelepathySet, Set.HopeSummersSet]

neStryfeScenario :: ScenarioCard NextEvolutionScenario
neStryfeScenario = scenario NextEvolutionScenario "40163" ("40163", "40164") ["40166", "40167"] [Set.NeStryfe, Set.HopeSummersSet]

instance RunMessage NextEvolutionScenario where
  runMessage msg (NextEvolutionScenario a) = runQueueT $ NextEvolutionScenario <$>
    case msg of
      StartScenario | scenarioId a == "40136" -> prepareMisterSinisterScenario a >>= runMessage StartScenario
      _ | scenarioId a `elem` ["40077", "40103"] -> runMaraudersScenarioMessage msg a
      _ -> runMessage msg a
