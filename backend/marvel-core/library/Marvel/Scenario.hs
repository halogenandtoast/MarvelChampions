module Marvel.Scenario where

import Marvel.Prelude

import Marvel.Card.Code
import Marvel.Game.Source
import Marvel.Message
import Marvel.Phase
import Marvel.Queue

data Scenario = TheBreakIn' TheBreakIn | KlawScenario' KlawScenario
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON)

instance RunMessage Scenario where
  runMessage = genericRunMessage

lookupScenario :: CardCode -> Maybe Scenario
lookupScenario = flip lookup allScenarios

allScenarios :: HashMap CardCode Scenario
allScenarios = fromList [("01097", TheBreakIn' rhinoScenario)]

newtype TheBreakIn = TheBreakIn ScenarioAttrs
  deriving newtype (Show, Eq, ToJSON, FromJSON)

instance RunMessage TheBreakIn where
  runMessage msg (TheBreakIn attrs) = TheBreakIn <$> runMessage msg attrs

rhinoScenario :: TheBreakIn
rhinoScenario = TheBreakIn $ ScenarioAttrs "01097" ["01094"]

newtype KlawScenario = KlawScenario ScenarioAttrs
  deriving newtype (Show, Eq, ToJSON, FromJSON)

instance RunMessage KlawScenario where
  runMessage msg (KlawScenario attrs) = KlawScenario <$> runMessage msg attrs

data ScenarioAttrs = ScenarioAttrs
  { scenarioId :: CardCode
  , scenarioVillains :: [CardCode]
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON)

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
