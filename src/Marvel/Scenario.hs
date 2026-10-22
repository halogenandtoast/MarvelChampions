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
    , SomeScenarioCard riskyBusinessScenario
    , SomeScenarioCard mutagenFormulaScenario

    -- The Wrecking Crew

    -- The Rise of Red Skull
    , SomeScenarioCard crossbonesScenario
    , SomeScenarioCard absorbingManScenario
    -- Zola
    , SomeScenarioCard zolaScenario

    -- Red Skull
    , SomeScenarioCard redSkullScenario

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
    , ("02001a", [EncounterSet.GoblinGimmicks])
    , ("02014", [EncounterSet.GoblinGimmicks])

    -- The Wrecking Crew

    -- The Rise of Red Skull
    , ("04058", [EncounterSet.HydraAssault, EncounterSet.WeaponMaster, EncounterSet.LegionsOfHydra])
    , ("04076", [EncounterSet.HydraPatrol])
    -- Zola
    , ("04109", [EncounterSet.UnderAttack])

    -- Red Skull
    , ("04125", [EncounterSet.HydraAssault, EncounterSet.HydraPatrol])

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

-- The Rise of Red Skull
-- Zola

newtype ZolaScenario = ZolaScenario (Attrs Scenario)
  deriving anyclass IsScenario
  deriving newtype (Show, Eq, ToJSON, FromJSON)

instance RunMessage ZolaScenario where
  runMessage msg (ZolaScenario a) = ZolaScenario <$> runMessage msg a

zolaScenario :: ScenarioCard ZolaScenario
zolaScenario = scenario
  ZolaScenario
  "04109"
  ("04109", "04110")
  ["04112", "04113"]
  [EncounterSet.Zola]

scenarioAccelerationTokens :: Scenario -> Natural
scenarioAccelerationTokens = Types.scenarioAccelerationTokens . toAttrs

-- The Green Goblin

newtype RiskyBusinessScenario = RiskyBusinessScenario (Attrs Scenario)
  deriving anyclass IsScenario
  deriving newtype (Show, Eq, ToJSON, FromJSON)

instance RunMessage RiskyBusinessScenario where
  runMessage msg (RiskyBusinessScenario a) = RiskyBusinessScenario <$> runMessage msg a

riskyBusinessScenario :: ScenarioCard RiskyBusinessScenario
riskyBusinessScenario = scenario
  RiskyBusinessScenario
  "02001a"
  ("02001a", "02002a")
  ["02004", "02005"]
  [EncounterSet.RiskyBusiness]

newtype MutagenFormulaScenario = MutagenFormulaScenario (Attrs Scenario)
  deriving anyclass IsScenario
  deriving newtype (Show, Eq, ToJSON, FromJSON)

instance RunMessage MutagenFormulaScenario where
  runMessage msg (MutagenFormulaScenario a) = MutagenFormulaScenario <$> runMessage msg a

mutagenFormulaScenario :: ScenarioCard MutagenFormulaScenario
mutagenFormulaScenario = scenario
  MutagenFormulaScenario
  "02014"
  ("02014", "02015")
  ["02017", "02018"]
  [EncounterSet.MutagenFormula]
-- The Rise of Red Skull

newtype CrossbonesScenario = CrossbonesScenario (Attrs Scenario)
  deriving anyclass IsScenario
  deriving newtype (Show, Eq, ToJSON, FromJSON)

instance RunMessage CrossbonesScenario where
  runMessage msg (CrossbonesScenario a) = CrossbonesScenario <$> runMessage msg a

-- | The Experimental Weapons set is made its own deck by Attack on Mount Athena's setup.
crossbonesScenario :: ScenarioCard CrossbonesScenario
crossbonesScenario = scenario
  CrossbonesScenario
  "04058"
  ("04058", "04059")
  ["04061", "04062", "04063"]
  [EncounterSet.Crossbones, EncounterSet.ExperimentalWeapons]

newtype AbsorbingManScenario = AbsorbingManScenario (Attrs Scenario)
  deriving anyclass IsScenario
  deriving newtype (Show, Eq, ToJSON, FromJSON)

instance RunMessage AbsorbingManScenario where
  runMessage msg (AbsorbingManScenario a) = AbsorbingManScenario <$> runMessage msg a

absorbingManScenario :: ScenarioCard AbsorbingManScenario
absorbingManScenario = scenario
  AbsorbingManScenario
  "04076"
  ("04076", "04077")
  ["04079"]
  [EncounterSet.AbsorbingMan]

-- Red Skull

newtype RedSkullScenario = RedSkullScenario (Attrs Scenario)
  deriving anyclass IsScenario
  deriving newtype (Show, Eq, ToJSON, FromJSON)

instance RunMessage RedSkullScenario where
  runMessage msg (RedSkullScenario a) = RedSkullScenario <$> runMessage msg a

redSkullScenario :: ScenarioCard RedSkullScenario
redSkullScenario = scenario
  RedSkullScenario
  "04125"
  ("04125", "04126")
  ["04128", "04129"]
  [EncounterSet.RedSkull]

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
