{-# OPTIONS_GHC -Wno-orphans #-}
module Marvel.Scenario where

import Marvel.Prelude

import Marvel.Card.Builder
import Data.HashMap.Strict qualified as HashMap
import Marvel.Card.Code
import Marvel.Difficulty
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
    [ SomeScenarioCard rhinoScenario
    , SomeScenarioCard klawScenario
    , SomeScenarioCard ultronScenario
    -- The Green Goblin

    -- The Wrecking Crew

    -- The Rise of Red Skull
    -- Taskmaster
    , SomeScenarioCard taskmasterScenario

    -- The Once and Future Kang

    ]

-- | The scenario with the sets it recommends, at standard difficulty.
recommendedSetup :: CardCode -> Maybe ScenarioSetup
recommendedSetup code = do
  _ <- lookup code allScenarios
  pure $ ScenarioSetup code (HashMap.findWithDefault [] code recommendedModulars) Normal

-- | The modular sets each scenario recommends: one, several, or none when not listed.
recommendedModulars :: HashMap CardCode [EncounterSet.EncounterSet]
recommendedModulars =
  fromList
    [ ("01094", [EncounterSet.BombScare])
    , ("01113", [EncounterSet.MastersOfEvil])
    , ("01134", [EncounterSet.UnderAttack])
    -- The Green Goblin

    -- The Wrecking Crew

    -- The Rise of Red Skull
    -- Taskmaster: Hydra Patrol is required, Weapon Master the modular
    , ("04093", [EncounterSet.WeaponMaster])

    -- The Once and Future Kang

    ]

lookupScenario :: ScenarioSetup -> Maybe Scenario
lookupScenario setup = case lookup setup.scenarioCode allScenarios of
  Just (SomeScenarioCard a) -> Just $ Scenario $ buildScenario a setup
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

newtype UltronScenario = UltronScenario (Attrs Scenario)
  deriving anyclass IsScenario
  deriving newtype (Show, Eq, ToJSON, FromJSON)

instance RunMessage UltronScenario where
  runMessage msg (UltronScenario a) = UltronScenario <$> runMessage msg a

ultronScenario :: ScenarioCard UltronScenario
ultronScenario = scenario
  UltronScenario
  "01134"
  ("01134", "01135")
  ["01137", "01138", "01139"]
  [EncounterSet.Ultron]

scenarioAccelerationTokens :: Scenario -> Natural
scenarioAccelerationTokens = Types.scenarioAccelerationTokens . toAttrs

-- The Rise of Red Skull

-- Taskmaster

newtype TaskmasterScenario = TaskmasterScenario (Attrs Scenario)
  deriving anyclass IsScenario
  deriving newtype (Show, Eq, ToJSON, FromJSON)

instance RunMessage TaskmasterScenario where
  runMessage msg (TaskmasterScenario a) = TaskmasterScenario <$> runMessage msg a

-- | Encounter deck: the Taskmaster and Hydra Patrol sets (Hydra Patrol "is required when
-- playing Taskmaster", TRORS rulebook p.10), a modular set and the standard set.
taskmasterScenario :: ScenarioCard TaskmasterScenario
taskmasterScenario = scenario
  TaskmasterScenario
  "04093"
  ("04093", "04094")
  ["04096"]
  [EncounterSet.Taskmaster, EncounterSet.HydraPatrol]
