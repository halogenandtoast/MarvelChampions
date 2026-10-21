{-# OPTIONS_GHC -Wno-orphans #-}
module Marvel.Scenario where

import Marvel.Prelude

import Marvel.Card.Builder
import Data.HashMap.Strict qualified as HashMap
import Marvel.Card.Code
import Marvel.Difficulty
import Marvel.EncounterSet (EncounterSet)
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

-- | The scenario with the set it recommends, at standard difficulty.
recommendedSetup :: CardCode -> Maybe ScenarioSetup
recommendedSetup code = do
  _ <- lookup code allScenarios
  modular <- lookup code recommendedModulars
  pure $ ScenarioSetup code [modular] Normal

recommendedModulars :: HashMap CardCode EncounterSet.EncounterSet
recommendedModulars =
  fromList
    [ ("01094", EncounterSet.BombScare)
    , ("01113", EncounterSet.MastersOfEvil)
    , ("01134", EncounterSet.UnderAttack)
    ]

lookupScenario :: ScenarioSetup -> Maybe Scenario
lookupScenario setup = case lookup setup.scenarioCode allScenarios of
  Just (SomeScenarioCard a) -> Just $ Scenario $ cbCardBuilder a setup
  Nothing -> Nothing

-- | The scenarios there are, by the code they are set up with.
scenarioCodes :: [CardCode]
scenarioCodes = HashMap.keys allScenarios

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
  ("01094", "01095")
  ["01097"]
  [EncounterSet.Rhino]

klawScenario :: ScenarioCard KlawScenario
klawScenario = scenario
  KlawScenario
  "01113"
  ("01113", "01114")
  ["01116", "01117"]
  [EncounterSet.Klaw]

scenarioAccelerationTokens :: Scenario -> Natural
scenarioAccelerationTokens = Types.scenarioAccelerationTokens . toAttrs
