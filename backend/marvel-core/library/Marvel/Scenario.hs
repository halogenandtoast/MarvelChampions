module Marvel.Scenario where

import Marvel.Prelude

import Marvel.Card.Code
import Marvel.Difficulty
import Marvel.EncounterSet qualified as EncounterSet
import Marvel.Entity
import Marvel.Message
import Marvel.Scenario.Attrs hiding (scenarioAccelerationTokens)
import Marvel.Scenario.Attrs qualified as Attrs

data Scenario = RhinoScenario' RhinoScenario | KlawScenario' KlawScenario
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON)

instance RunMessage Scenario where
  runMessage = genericRunMessage

lookupScenario :: CardCode -> Maybe Scenario
lookupScenario = flip lookup allScenarios

allScenarios :: HashMap CardCode Scenario
allScenarios = fromList [("01094", RhinoScenario' rhinoScenario), ("01113", KlawScenario' klawScenario)]

newtype RhinoScenario = RhinoScenario ScenarioAttrs
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

instance RunMessage RhinoScenario where
  runMessage msg (RhinoScenario a) = RhinoScenario <$> runMessage msg a

newtype KlawScenario = KlawScenario ScenarioAttrs
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

instance RunMessage KlawScenario where
  runMessage msg (KlawScenario a) = KlawScenario <$> runMessage msg a

rhinoScenario :: RhinoScenario
rhinoScenario = scenario
  RhinoScenario
  "01094"
  ["01094"]
  ["01097"]
  [EncounterSet.Rhino, EncounterSet.BombScare, EncounterSet.Standard]

klawScenario :: KlawScenario
klawScenario = scenario
  KlawScenario
  "01113"
  ["01113"]
  ["01116", "01117"]
  [EncounterSet.Klaw, EncounterSet.MastersOfEvil, EncounterSet.Standard]

instance Entity Scenario where
  type EntityId Scenario = CardCode
  type EntityAttrs Scenario = ScenarioAttrs
  toId = toId . toAttrs
  toAttrs = genericToAttrs

getScenarioDifficulty :: Scenario -> Difficulty
getScenarioDifficulty = scenarioDifficulty . toAttrs

scenarioAccelerationTokens :: Scenario -> Natural
scenarioAccelerationTokens = Attrs.scenarioAccelerationTokens . toAttrs
