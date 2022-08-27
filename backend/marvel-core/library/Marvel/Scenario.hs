{-# OPTIONS_GHC -Wno-orphans #-}
module Marvel.Scenario where

import Marvel.Prelude

import Marvel.Card.Builder
import Marvel.Card.Code
import Marvel.EncounterSet qualified as EncounterSet
import Marvel.Entity
import Marvel.Message
import Marvel.Scenario.Types hiding (scenarioAccelerationTokens)
import Marvel.Scenario.Types qualified as Types

instance FromJSON Scenario where
  parseJSON = withObject "Scenario" $ \o -> do
    cardCode <- o .: "scenarioId"
    withScenarioCardCode cardCode
      $ \(_ :: ScenarioCard a) -> Scenario <$> parseJSON @a (Object o)

withScenarioCardCode
  :: CardCode -> (forall a . IsScenario a => ScenarioCard a -> r) -> r
withScenarioCardCode cCode f = case lookup cCode allScenarios of
  Nothing -> error "invalid minion"
  Just (SomeScenarioCard a) -> f a

allScenarios :: HashMap CardCode SomeScenarioCard
allScenarios = fromList $ map
  (toFst someScenarioCardCode)
  [SomeScenarioCard rhinoScenario, SomeScenarioCard klawScenario]

lookupScenario :: CardCode -> Maybe Scenario
lookupScenario cCode = case lookup cCode allScenarios of
  Just (SomeScenarioCard a) -> Just $ Scenario $ cbCardBuilder a ()
  Nothing -> Nothing

newtype RhinoScenario = RhinoScenario (Attrs Scenario)
  deriving anyclass IsScenario
  deriving newtype (Show, Eq, ToJSON, FromJSON)

instance RunMessage RhinoScenario where
  runMessage msg (RhinoScenario a) = RhinoScenario <$> runMessage msg a

newtype KlawScenario = KlawScenario (Attrs Scenario)
  deriving anyclass IsScenario
  deriving newtype (Show, Eq, ToJSON, FromJSON)

instance RunMessage KlawScenario where
  runMessage msg (KlawScenario a) = KlawScenario <$> runMessage msg a

rhinoScenario :: ScenarioCard RhinoScenario
rhinoScenario = scenario
  RhinoScenario
  "01094"
  ["01094"]
  ["01097"]
  [EncounterSet.Rhino, EncounterSet.BombScare, EncounterSet.Standard]

klawScenario :: ScenarioCard KlawScenario
klawScenario = scenario
  KlawScenario
  "01113"
  ["01113"]
  ["01116", "01117"]
  [EncounterSet.Klaw, EncounterSet.MastersOfEvil, EncounterSet.Standard]

scenarioAccelerationTokens :: Scenario -> Natural
scenarioAccelerationTokens = Types.scenarioAccelerationTokens . toAttrs
