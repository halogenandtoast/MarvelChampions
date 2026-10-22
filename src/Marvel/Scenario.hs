{-# OPTIONS_GHC -Wno-orphans #-}
module Marvel.Scenario where

import Marvel.Prelude

import Marvel.Card.Builder
import Data.HashMap.Strict qualified as HashMap
import Marvel.Card.Code
import Marvel.Difficulty
import Marvel.EncounterSet qualified as EncounterSet
import Marvel.EncounterCard (isPlayerObligation)
import Marvel.Entity
import Marvel.Game.Source
import Marvel.Message.Lifted
import Marvel.Ref
import Data.Aeson (Result (..), fromJSON)
import Marvel.Matchers (IdentityMatcher (FirstPlayer))
import Marvel.Query (selectJust)
import Marvel.Question (Choice (CardLabel))
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
    , SomeScenarioCard (breakoutScenario "07001")
    , SomeScenarioCard (breakoutScenario "07001a")
    , SomeScenarioCard (breakoutScenario "07001b")

    -- The Rise of Red Skull
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

-- | Breakout (The Wrecking Crew): the four villains, each with his own side scheme and
-- encounter deck and no other encounter sets; version A for standard, B for expert
-- (TWC insert p.7). Set up by the A side's code, it is the extreme variant: version A
-- with version B under it ('versionBEnters').
newtype BreakoutScenario = BreakoutScenario (Attrs Scenario)
  deriving anyclass IsScenario
  deriving newtype (Show, Eq, ToJSON, FromJSON)

breakoutScenario :: CardCode -> ScenarioCard BreakoutScenario
breakoutScenario code =
  ScenarioCard code \setup ->
    let BreakoutScenario a = buildScenario (scenario BreakoutScenario code ("07002", "07003") ["07001"] []) setup
        expert = setup.difficulty == Expert && code == "07001"
     in BreakoutScenario
          a
            { Types.scenarioVillains = if expert then ["07003", "07018", "07033", "07047"] else ["07002", "07017", "07032", "07046"]
            , Types.scenarioEncounterSets = mempty
            }

-- | Wrecker holds the active counter at setup (Breakout 1A). When the active villain is
-- defeated with others left, the counter goes to the villain whose side scheme has the
-- most threat, the first player choosing among those tied (TWC insert p.6); the players
-- win once all four are defeated (p.7).
-- | Each villain's versions, A and B, in the order they are set out.
versions :: [(CardCode, CardCode)]
versions = [("07002", "07003"), ("07017", "07018"), ("07032", "07033"), ("07046", "07047")]

instance RunMessage BreakoutScenario where
  runMessage msg (BreakoutScenario a) = BreakoutScenario <$> case msg of
    -- set up by the B side's code, the players choose each villain's version, A or B
    -- ("some version-A and some version-B villains", TWC insert p.7)
    StartScenario | Types.scenarioId a == "07001b" -> runQueueT do
      cardSpecific GameRef "breakoutVersions" versions
      pure $ a & mainSchemesL .~ []
    CardSpecific GameRef "breakoutVersions" (fromJSON -> Success (remaining :: [(CardCode, CardCode)])) -> runQueueT do
      case remaining of
        [] -> addSetupMainScheme "07001"
        (versionA, versionB) : rest -> do
          first <- selectJust FirstPlayer
          let version code = CardLabel code <$> labeled "$label.thisVersion" do
                addVillain code
                cardSpecific GameRef "breakoutVersions" rest
          chooseOnePrompted first "$label.chooseVillainVersion" [version versionA, version versionB]
      pure a
    StartScenario -> runQueueT do
      for_ (Types.scenarioVillains a) addVillain
      addSetupMainScheme "07001"
      pure $ a & mainSchemesL .~ []
    ResolveVillainDefeat vid -> runQueueT do
      aid <- getAreaOfTarget vid
      wasActive <- (== Just vid) <$> getActiveVillainIn aid
      others <- filter (/= vid) <$> getVillainsInArea aid
      if null others
        then gameOver Won
        else do
          removeFromPlay (VillainRef vid)
          when wasActive $ cardSpecific GameRef "breakoutNextActive" ()
      pure a
    CardSpecific GameRef "breakoutNextActive" _ -> runQueueT $ a <$ moveActiveCounter MostThreat
    -- a defeated villain's deck and discard pile stay: "when an encounter card leaves
    -- play, it is placed in the discard pile of its corresponding encounter deck", his
    -- cards still in play remaining in play (TWC insert pp.6-7); only his side scheme is
    -- removed. It is not drawn from again, being no active villain's.
    RemoveDeck (VillainDeckKey _) -> pure a
    -- the obligations are the encounter deck's only cards: emptied of them, it is not an
    -- encounter deck run out
    RemovePlayerObligationsFromGame -> pure $ a & encounterDeckL %~ filter (not . isPlayerObligation)
    _ -> runMessage msg a

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
