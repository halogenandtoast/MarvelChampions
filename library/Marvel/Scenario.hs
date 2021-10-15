module Marvel.Scenario where

import Marvel.Prelude

import Marvel.Card.Code
import {-# SOURCE #-} Marvel.Game
import Marvel.Message
import Marvel.Phase
import Marvel.Queue

data Scenario = RhinoScenario' RhinoScenario | KlawScenario' KlawScenario
  deriving stock (Show, Generic)

instance RunMessage Scenario where
  runMessage = genericRunMessage

lookupScenario :: CardCode -> Maybe Scenario
lookupScenario = flip lookup allScenarios

allScenarios :: HashMap CardCode Scenario
allScenarios = fromList [("01094", RhinoScenario' rhinoScenario)]

newtype RhinoScenario = RhinoScenario ScenarioAttrs
  deriving newtype Show

instance RunMessage RhinoScenario where
  runMessage msg (RhinoScenario attrs) = RhinoScenario <$> runMessage msg attrs

rhinoScenario :: RhinoScenario
rhinoScenario = RhinoScenario $ ScenarioAttrs ["01094"]

newtype KlawScenario = KlawScenario ScenarioAttrs
  deriving newtype Show

instance RunMessage KlawScenario where
  runMessage msg (KlawScenario attrs) = KlawScenario <$> runMessage msg attrs

newtype ScenarioAttrs = ScenarioAttrs
  { scenarioVillains :: [CardCode]
  }
  deriving stock Show

instance RunMessage ScenarioAttrs where
  runMessage msg attrs@ScenarioAttrs {..} = case msg of
    StartScenario -> do
      pushAll $ map AddVillain scenarioVillains <> [BeginPhase PlayerPhase]
      pure attrs
    BeginPhase PlayerPhase -> do
      players <- getPlayers
      pushAll $ map (($ BeginTurn) . IdentityMessage) players
      pure attrs
    _ -> pure attrs
