{-# OPTIONS_GHC -Wno-orphans #-}
module Marvel.Scenario where

import Marvel.Prelude

import Marvel.Card.Builder
import Data.HashMap.Strict qualified as HashMap
import Marvel.Area (experimentalWeaponsDeck, newArea)
import Marvel.Attachment.Cards (allAttachments)
import Marvel.Campaign (CampaignRecord (RecordExperimentalWeapons), CampaignStepKind (CampaignVictory))
import Marvel.Card.Code
import Marvel.Card.Def
import Marvel.Card.EncounterCard (EncounterCard)
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
import Marvel.Scenario.Types
import Marvel.Scenario.Types qualified as Types
import Data.List (find)
import Marvel.MainScheme.Cards qualified as MainSchemes
import Marvel.MainScheme.MainSchemes.KangsWrath (kangsDominionDeck)
import Marvel.SideScheme.Cards qualified as SideSchemes
import Marvel.Matchers (IdentityMatcher (FirstPlayer))
import Marvel.Query (selectJust)
import Marvel.Card (Card (EncounterCard))
import Marvel.Id (MainSchemeId)
import Marvel.Name (Name (..))

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
    , SomeScenarioCard crossbonesScenario
    , SomeScenarioCard absorbingManScenario
    -- Zola
    , SomeScenarioCard zolaScenario

    -- Red Skull
    , SomeScenarioCard redSkullScenario

    -- Taskmaster
    , SomeScenarioCard taskmasterScenario

    -- The Once and Future Kang
    , SomeScenarioCard kangScenario

    -- The Galaxy's Most Wanted: Escape the Museum
    , SomeScenarioCard escapeTheMuseumScenario

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
    , ("11001", [EncounterSet.Temporal])

    -- The Galaxy's Most Wanted: Escape the Museum
    , ("16080a", [EncounterSet.MenagerieMedley])

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

-- | Victory: "Record the name of each Experimental attachment that entered the game":
-- each not still in the Experimental Weapons deck. Then the default victory steps.
instance RunMessage CrossbonesScenario where
  runMessage msg (CrossbonesScenario a) = case msg of
    CampaignStep CampaignVictory -> do
      let Pile left _ = a ^. pileL experimentalWeaponsDeck
          experimental = [cdCardCode d | d <- HashMap.elems allAttachments, cdEncounterSet d == Just EncounterSet.ExperimentalWeapons]
          entered = filter (`notElem` map toCardCode left) experimental
      runQueueT do
        unless (null entered) $ recordInCampaignLog (RecordExperimentalWeapons entered)
        CrossbonesScenario <$> runMessage msg a
    _ -> CrossbonesScenario <$> runMessage msg a

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

-- The Once and Future Kang

-- | Kang (TOAFK insert pp.3-7): Kang (I) is the villain deck's only villain; each Kang
-- (II) and Kang (III) enters play with his stage. Stage 2A splits the players into game
-- areas of their own; a player whose Kang (II) is defeated, or whose stage 3 is
-- completed, joins another area at the end of the phase, and the players meet again at
-- The Master of Time (2B), which advances to stage 4A once they all have.
newtype KangScenario = KangScenario (Attrs Scenario)
  deriving anyclass IsScenario
  deriving newtype (Show, Eq, ToJSON, FromJSON)

kangScenario :: ScenarioCard KangScenario
kangScenario = scenario KangScenario "11001" ("11001", "11034") ["11007", "11008", "11013"] [EncounterSet.Kang]

instance RunMessage KangScenario where
  runMessage msg (KangScenario a) = KangScenario <$> case msg of
    -- a stage 3 completed: a set-aside Kang's Dominion goes facedown under stage 4A
    CardSpecific GameRef "kangDominionUnderStage4" _ -> runQueueT do
      let Pile under _ = a ^. pileL kangsDominionDeck
          dominions = [ec | EncounterCard ec <- scenarioSetAsideCards a, toCardCode ec == toCardCode SideSchemes.kangsDominion]
      for_ (take 1 dominions) \d -> createDeck kangsDominionDeck (under <> [d])
      pure a
    CardSpecific GameRef "kangJoinAnotherArea" (fromJSON -> Success aid) -> runQueueT $ a <$ joinAnotherArea aid
    CardSpecific GameRef "kangAllJoined" _ -> runQueueT do
      central <- masterOfTimeArea
      players <- getAllPlayers
      for_ central \(aid, mid) -> do
        here <- areaPlayers <$> getAreaOfArea aid
        when (all (`elem` here) players) $ nextMainScheme mid
      pure a
    _ -> runMessage msg a

-- | The area The Master of Time (2B) is in, and the scheme, while it is in play.
masterOfTimeArea :: (HasGame m) => m (Maybe (AreaId, MainSchemeId))
masterOfTimeArea = do
  areas <- getAreas
  found <- for areas \area -> do
    schemes <- filterM (fmap (== Just (toCardCode MainSchemes.theMasterOfTime)) . getMainSchemeCode) =<< getMainSchemesIn (areaId area)
    pure [(areaId area, s) | s <- schemes]
  pure $ headMay (concat found)

getAreaOfArea :: (HasGame m) => AreaId -> m Area
getAreaOfArea aid = fromMaybe (newArea aid []) . find ((== aid) . areaId) <$> getAreas

-- | The area's players join another game area: its players choose which (the first of
-- them in turn order, or the first player when none is left), and what was in it joins
-- them there; The Master of Time's area may be joined only once no other is left.
joinAnotherArea :: (ReverseQueue m) => AreaId -> m ()
joinAnotherArea aid = do
  areas <- getAreas
  central <- fmap fst <$> masterOfTimeArea
  for_ (find ((== aid) . areaId) areas) \here -> do
    let others = filter ((/= aid) . areaId) areas
        open = filter ((/= central) . Just . areaId) others
        choices = if null open then others else open
    order <- getAllPlayers
    chooser <- maybe (selectJust FirstPlayer) pure $ headMay (filter (`elem` areaPlayers here) order)
    targets <- for choices \area -> do
      schemes <- getMainSchemesIn (areaId area)
      pure $ fromMaybe GameRef $ headMay $ map MainSchemeRef schemes <> map VillainRef (areaVillains area) <> map IdentityRef (areaPlayers area)
    unless (null choices) $
      chooseOrRunOne chooser
        [ targeting target do
            combineAreas aid (areaId area)
            cardSpecific GameRef "kangAllJoined" ()
        | (area, target) <- zip choices targets
        ]

-- The Galaxy's Most Wanted: Escape the Museum

-- | Escape the Museum (TGMW rulebook pp.11-12): Collector (A1), or (B1) for expert play;
-- the Escape the Museum, Galactic Artifacts and Ship Command sets, with Menagerie Medley
-- its modular set. The Missing Milano's setup sets the Ship Command set aside.
newtype EscapeTheMuseumScenario = EscapeTheMuseumScenario (Attrs Scenario)
  deriving anyclass IsScenario
  deriving newtype (Show, Eq, ToJSON, FromJSON)

instance RunMessage EscapeTheMuseumScenario where
  runMessage msg (EscapeTheMuseumScenario a) = EscapeTheMuseumScenario <$> runMessage msg a

escapeTheMuseumScenario :: ScenarioCard EscapeTheMuseumScenario
escapeTheMuseumScenario =
  scenario
    EscapeTheMuseumScenario
    "16080a"
    ("16080a", "16081a")
    ["16082", "16083", "16084"]
    [EncounterSet.EscapeTheMuseum, EncounterSet.GalacticArtifacts, EncounterSet.ShipCommand]

-- | For the campaign log (Escape the Museum's Victory): the titles of the Galactic
-- Artifacts side schemes in the victory display.
galacticArtifactsInVictoryDisplay :: (HasGame m) => m [Text]
galacticArtifactsInVictoryDisplay = do
  cards <- getVictoryDisplay
  pure
    [ title (cdName d)
    | c <- cards
    , let d = getCardDef c
    , cdCardType d == SideSchemeType
    , cdEncounterSet d == Just EncounterSet.GalacticArtifacts
    ]
