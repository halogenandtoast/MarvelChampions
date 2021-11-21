module Marvel.Scenario where

import Marvel.Prelude

import Marvel.Card.Code
import Marvel.Difficulty
import Marvel.EncounterSet qualified as EncounterSet
import Marvel.Entity
import Marvel.GameValue
import Marvel.Message
import Marvel.Queue
import Marvel.Scenario.Attrs

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
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

instance RunMessage TheBreakIn where
  runMessage msg s@(TheBreakIn attrs) = case msg of
    AdvanceScenario -> s <$ push (GameOver Lost)
    _ -> TheBreakIn <$> runMessage msg attrs

rhinoScenario :: TheBreakIn
rhinoScenario = scenario
  TheBreakIn
  "01097"
  ["01094"]
  [EncounterSet.Rhino]
  (PerPlayer 7)
  (Static 0)
  (PerPlayer 1)

newtype KlawScenario = KlawScenario ScenarioAttrs
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

instance RunMessage KlawScenario where
  runMessage msg (KlawScenario attrs) = KlawScenario <$> runMessage msg attrs

instance Entity Scenario where
  type EntityId Scenario = CardCode
  type EntityAttrs Scenario = ScenarioAttrs
  toId = toId . toAttrs
  toAttrs = genericToAttrs

getScenarioDifficulty :: Scenario -> Difficulty
getScenarioDifficulty = scenarioDifficulty . toAttrs
