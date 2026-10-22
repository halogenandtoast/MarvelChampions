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
    , SomeScenarioCard (breakoutScenario "07001")
    , SomeScenarioCard (breakoutScenario "07001a")

    -- The Rise of Red Skull

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
        expert = setup.difficulty == Expert && code /= "07001a"
     in BreakoutScenario
          a
            { Types.scenarioVillains = if expert then ["07003", "07018", "07033", "07047"] else ["07002", "07017", "07032", "07046"]
            , Types.scenarioEncounterSets = mempty
            }

-- | Wrecker holds the active counter at setup (Breakout 1A). When the active villain is
-- defeated with others left, the counter goes to the villain whose side scheme has the
-- most threat, the first player choosing among those tied (TWC insert p.6); the players
-- win once all four are defeated (p.7).
instance RunMessage BreakoutScenario where
  runMessage msg (BreakoutScenario a) = BreakoutScenario <$> case msg of
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
    -- the obligations are the encounter deck's only cards: emptied of them, it is not an
    -- encounter deck run out
    RemovePlayerObligationsFromGame -> pure $ a & encounterDeckL %~ filter (not . isPlayerObligation)
    _ -> runMessage msg a

scenarioAccelerationTokens :: Scenario -> Natural
scenarioAccelerationTokens = Types.scenarioAccelerationTokens . toAttrs
