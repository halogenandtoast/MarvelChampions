{-# OPTIONS_GHC -Wno-orphans #-}
module Marvel.Scenario where

import Marvel.Scenario.AgentsOfShield

import Marvel.Scenario.TricksterTakeover

import Marvel.Scenario.AgeOfApocalypseDarkBeast
import Marvel.Scenario.AgeOfApocalypseEnSabahNur
import Marvel.Scenario.AgeOfApocalypseApocalypse
import Marvel.Scenario.AgeOfApocalypseFourHorsemen
import Marvel.Scenario.AgeOfApocalypseUnus
import Marvel.Scenario.NextEvolution

import Marvel.Scenario.Mojo
import Marvel.Scenario.MojoOpening
import Data.HashSet qualified as HashSet
import Data.List ( nub, find )
import Marvel.Query (selectOne, selectList, selectJust)
import Marvel.Matchers (VillainMatcher (ActiveVillain), IdentityMatcher (FirstPlayer), SideSchemeMatcher (AnySideScheme, SideSchemeIs), SchemeMatcher (SchemeWithId, EncounterMainScheme, MainScheme))
import Marvel.Prelude
import Marvel.Scenario.MutantGenesis

import Data.HashMap.Strict qualified as HashMap
import Marvel.Area (experimentalWeaponsDeck, newArea)
import Marvel.Attachment.Cards (allAttachments)
import Marvel.Campaign (CampaignRecord (RecordExperimentalWeapons), CampaignStepKind (CampaignVictory))
import Marvel.Card.Id (CardId (..))
import Marvel.MainScheme.MainSchemes.SmVenomGoblin qualified as Goblin
import Marvel.Environment.Cards qualified as Environments
import Marvel.Card.Code
import Marvel.Card.Def
import Marvel.Card.EncounterCard (EncounterCard (..))
import Marvel.Difficulty
import Marvel.EncounterSet qualified as EncounterSet
import Marvel.EncounterCard (isPlayerObligation, genEncounterCard, gatherEncounterSet)
import Marvel.Entity
import Marvel.Game.Source
import Marvel.Villain.Villains.SmSinisterSix qualified as Six
import Marvel.Modifier (Modifier (ThreatCannotBeRemoved))
import Marvel.Message.Lifted
import Marvel.Ref
import Data.Aeson ()
import Marvel.Question (Choice (CardLabel))
import Marvel.Scenario.Types
import Marvel.Scenario.Types qualified as Types
import Marvel.MainScheme.Cards qualified as MainSchemes
import Marvel.MainScheme.MainSchemes.KangsWrath (kangsDominionDeck)
import Marvel.SideScheme.Cards qualified as SideSchemes
import Marvel.Card (Card (EncounterCard))
import Marvel.Id (MainSchemeId (..), VillainId, IdentityId, SchemeId (..))
import Marvel.Name (Name (..))
import Marvel.Villain.Cards qualified as VillainCards
import System.Random.Shuffle (shuffleM)

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
    [ SomeScenarioCard aosBlackWidowScenario, SomeScenarioCard aosBatrocScenario, SomeScenarioCard aosModokScenario, SomeScenarioCard aosThunderboltsScenario, SomeScenarioCard aosBaronZemoScenario
  , SomeScenarioCard aoaDarkBeastScenario, SomeScenarioCard aoaEnSabahNurScenario, SomeScenarioCard aoaApocalypseScenario, SomeScenarioCard aoaFourHorsemenScenario, SomeScenarioCard aoaUnusScenario
    , SomeScenarioCard neMorlockSiegeScenario, SomeScenarioCard neOnTheRunScenario, SomeScenarioCard neJuggernautScenario, SomeScenarioCard neMisterSinisterScenario, SomeScenarioCard neStryfeScenario
    , SomeScenarioCard enchantressScenario
    , SomeScenarioCard godOfLiesScenario
    , SomeScenarioCard mojoScenario
    , SomeScenarioCard mojoMagogScenario
    , SomeScenarioCard mojoSpiralScenario
    , SomeScenarioCard mgSabretoothScenario
    , SomeScenarioCard mgProjectWideawakeScenario
    , SomeScenarioCard mgMasterMoldScenario
    , SomeScenarioCard mgMansionAttackScenario
    , SomeScenarioCard mgMagnetoScenario
    , SomeScenarioCard theHoodScenario
    , SomeScenarioCard rhinoScenario
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

    -- The Galaxy's Most Wanted: Brotherhood of Badoon
    , SomeScenarioCard brotherhoodOfBadoonScenario

    -- The Galaxy's Most Wanted: Escape the Museum
    , SomeScenarioCard escapeTheMuseumScenario

    -- Ronan the Accuser
    , SomeScenarioCard ronanScenario
    -- The Galaxy's Most Wanted: Nebula
    , SomeScenarioCard nebulaScenario
    -- The Galaxy's Most Wanted: Infiltrate the Museum
    , SomeScenarioCard infiltrateTheMuseumScenario
    -- The Mad Titan's Shadow: Ebony Maw
    , SomeScenarioCard ebonyMawScenario
    -- The Mad Titan's Shadow: Tower Defense
    , SomeScenarioCard towerDefenseScenario
    -- The Mad Titan's Shadow: Thanos
    , SomeScenarioCard thanosScenario
    -- The Mad Titan's Shadow: Loki
    , SomeScenarioCard lokiScenario
    -- The Mad Titan's Shadow: Hela
    , SomeScenarioCard helaScenario
    -- Sinister Motives
    , SomeScenarioCard smSandmanScenario
    , SomeScenarioCard smVenomScenario
    , SomeScenarioCard smMysterioScenario
    , SomeScenarioCard smSinisterSixScenario
    , SomeScenarioCard smVenomGoblinScenario
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
    [ ("55001", [EncounterSet.TricksterMagic]), ("55027a", [EncounterSet.TricksterMagic])
    , ("50064", [EncounterSet.AosAimAbduction, EncounterSet.AosAimScience])
    , ("50086a", [EncounterSet.AosAimScience, EncounterSet.AosBatrocsBrigade])
    , ("50103a", [EncounterSet.AosScientistSupreme])
    , ("50129a", [EncounterSet.AosGravitationalPull, EncounterSet.AosHardSound])
    , ("50165a", [EncounterSet.AosScientistSupreme, EncounterSet.AosShield])
    , ("45118", [EncounterSet.AoaDystopianNightmare])
    , ("45184a", [EncounterSet.AoaCelestialTech, EncounterSet.AoaClanAkkaba])
    , ("45101b", [EncounterSet.AoaDarkRiders, EncounterSet.AoaInfinites])
    , ("45081a", [EncounterSet.AoaDystopianNightmare, EncounterSet.AoaHounds])
    , ("45059", [EncounterSet.AoaDystopianNightmare])
    , ("40077", [EncounterSet.MilitaryGrade, EncounterSet.MutantSlayers]), ("40103", [EncounterSet.MilitaryGrade, EncounterSet.NastyBoys]), ("40118", [EncounterSet.BlackTomCassidy]), ("40136", [EncounterSet.NastyBoys]), ("40163", [EncounterSet.ExtremeMeasures, EncounterSet.MutantInsurrection])
    , ("32060", [EncounterSet.Brotherhood, EncounterSet.Mystique])
    , ("32084", [EncounterSet.Sentinels])
    , ("32109", [EncounterSet.ZeroTolerance])
    , ("32121a", [EncounterSet.Mystique])
    , ("32138", [EncounterSet.Acolytes])
    , ("01094", [EncounterSet.BombScare])
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

    -- The Galaxy's Most Wanted: Brotherhood of Badoon
    , ("16058", [EncounterSet.BandOfBadoon])

    -- The Galaxy's Most Wanted: Escape the Museum
    , ("16080a", [EncounterSet.MenagerieMedley])

    -- Ronan the Accuser
    , ("16103", [EncounterSet.KreeMilitants])
    -- The Galaxy's Most Wanted: Nebula
    , ("16088", [EncounterSet.SpacePirates])
    -- The Galaxy's Most Wanted: Infiltrate the Museum
    , ("16070", [EncounterSet.MenagerieMedley])
    -- The Mad Titan's Shadow: Ebony Maw
    , ("21071", [EncounterSet.BlackOrderSet, EncounterSet.ArmiesOfTitan])
    -- The Mad Titan's Shadow: Tower Defense
    , ("21098", [EncounterSet.ArmiesOfTitan])
    -- The Mad Titan's Shadow: Thanos
    , ("21111", [EncounterSet.BlackOrderSet, EncounterSet.ChildrenOfThanos])
    -- The Mad Titan's Shadow: Loki
    , ("21160", [EncounterSet.Enchantress, EncounterSet.FrostGiants])
    -- The Mad Titan's Shadow: Hela
    , ("21136a", [EncounterSet.LegionsOfHel, EncounterSet.FrostGiants])
    -- Sinister Motives
    , ("27061", [EncounterSet.DownToEarth])
    , ("27073", [EncounterSet.DownToEarth])
    , ("27084", [EncounterSet.WhispersOfParanoia])
    , ("27100", [EncounterSet.GuerrillaTactics])
    , ("27113", [EncounterSet.GoblinGear])
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

-- The Galaxy's Most Wanted: Brotherhood of Badoon

-- | Brotherhood of Badoon (TGMW rulebook p.8): Drang (I) and (II), (II) and (III) in
-- expert; Terrestrial Invasion and Protect the Planet; the Brotherhood of Badoon, Ship
-- Command and standard sets, Band of Badoon the modular.
newtype BrotherhoodOfBadoonScenario = BrotherhoodOfBadoonScenario (Attrs Scenario)
  deriving anyclass IsScenario
  deriving newtype (Show, Eq, ToJSON, FromJSON)

instance RunMessage BrotherhoodOfBadoonScenario where
  runMessage msg (BrotherhoodOfBadoonScenario a) = BrotherhoodOfBadoonScenario <$> runMessage msg a

brotherhoodOfBadoonScenario :: ScenarioCard BrotherhoodOfBadoonScenario
brotherhoodOfBadoonScenario =
  scenario
    BrotherhoodOfBadoonScenario
    "16058"
    ("16058", "16059")
    ["16061", "16062"]
    [EncounterSet.BrotherhoodOfBadoon, EncounterSet.ShipCommand]

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

-- Ronan the Accuser

newtype RonanScenario = RonanScenario (Attrs Scenario)
  deriving anyclass IsScenario
  deriving newtype (Show, Eq, ToJSON, FromJSON)

instance RunMessage RonanScenario where
  runMessage msg (RonanScenario a) = RonanScenario <$> runMessage msg a

-- | Ronan the Accuser (TGMW rulebook p.18): Ronan (I) and (II), or (II) and (III) for
-- expert; the Ronan the Accuser, Power Stone and Ship Command sets, and Kree Militants
-- as the modular set.
ronanScenario :: ScenarioCard RonanScenario
ronanScenario =
  scenario
    RonanScenario
    "16103"
    ("16103", "16104")
    ["16106", "16107"]
    [EncounterSet.Ronan, EncounterSet.PowerStone, EncounterSet.ShipCommand]

-- The Galaxy's Most Wanted: Nebula

newtype NebulaScenario = NebulaScenario (Attrs Scenario)
  deriving anyclass IsScenario
  deriving newtype (Show, Eq, ToJSON, FromJSON)

instance RunMessage NebulaScenario where
  runMessage msg (NebulaScenario a) = NebulaScenario <$> runMessage msg a

-- | Nebula (TGMW rulebook p.14): Nebula (I) and (II), (II) and (III) for expert; the
-- Nebula, Power Stone and Ship Command sets, and the Space Pirates modular.
nebulaScenario :: ScenarioCard NebulaScenario
nebulaScenario =
  scenario
    NebulaScenario
    "16088"
    ("16088", "16089")
    ["16091", "16092"]
    [EncounterSet.Nebula, EncounterSet.PowerStone, EncounterSet.ShipCommand]

-- The Galaxy's Most Wanted: Infiltrate the Museum

-- | Collector I and II (II and III in expert mode), The Grand Collection, and the
-- Infiltrate the Museum and Galactic Artifacts sets (TGMW rulebook p.10): Menagerie
-- Medley is its modular set, and may be changed.
newtype InfiltrateTheMuseumScenario = InfiltrateTheMuseumScenario (Attrs Scenario)
  deriving anyclass IsScenario
  deriving newtype (Show, Eq, ToJSON, FromJSON)

instance RunMessage InfiltrateTheMuseumScenario where
  runMessage msg (InfiltrateTheMuseumScenario a) = InfiltrateTheMuseumScenario <$> runMessage msg a

infiltrateTheMuseumScenario :: ScenarioCard InfiltrateTheMuseumScenario
infiltrateTheMuseumScenario = scenario
  InfiltrateTheMuseumScenario
  "16070"
  ("16070", "16071")
  ["16073"]
  [EncounterSet.InfiltrateTheMuseum, EncounterSet.GalacticArtifacts]

-- The Mad Titan's Shadow: Ebony Maw

newtype EbonyMawScenario = EbonyMawScenario (Attrs Scenario)
  deriving anyclass IsScenario
  deriving newtype (Show, Eq, ToJSON, FromJSON)

instance RunMessage EbonyMawScenario where
  runMessage msg (EbonyMawScenario a) = EbonyMawScenario <$> runMessage msg a

-- | Ebony Maw (MTS rulebook p.6): Ebony Maw (I) and (II), (II) and (III) for expert;
-- Attack on Knowhere and The Power Stone; the Ebony Maw set, with Black Order and
-- Armies of Titan as its modular sets.
ebonyMawScenario :: ScenarioCard EbonyMawScenario
ebonyMawScenario =
  scenario
    EbonyMawScenario
    "21071"
    ("21071", "21072")
    ["21074", "21075"]
    [EncounterSet.EbonyMaw]

-- The Mad Titan's Shadow: Tower Defense

-- | Tower Defense (MTS rulebook p.10): Proxima Midnight and Corvus Glaive, (I) and (II),
-- or (II) and (III) for expert, both in play from the start, Proxima first; Under Siege,
-- which reveals The Armies of Thanos beside it; the Tower Defense set, with Armies of
-- Titan the modular. Set up by Under Siege's code, as Breakout is by its own.
newtype TowerDefenseScenario = TowerDefenseScenario (Attrs Scenario)
  deriving anyclass IsScenario
  deriving newtype (Show, Eq, ToJSON, FromJSON)

instance RunMessage TowerDefenseScenario where
  runMessage msg (TowerDefenseScenario a) = TowerDefenseScenario <$> runMessage msg a

towerDefenseScenario :: ScenarioCard TowerDefenseScenario
towerDefenseScenario =
  ScenarioCard "21098" \setup ->
    let TowerDefenseScenario a = buildScenario (scenario TowerDefenseScenario "21098" ("21092", "21093") ["21098"] [EncounterSet.TowerDefense]) setup
        expert = setup.difficulty == Expert
     in TowerDefenseScenario a {Types.scenarioVillains = if expert then ["21093", "21096"] else ["21092", "21095"]}

-- The Mad Titan's Shadow: Thanos

newtype ThanosScenario = ThanosScenario (Attrs Scenario)
  deriving anyclass IsScenario
  deriving newtype (Show, Eq, ToJSON, FromJSON)

instance RunMessage ThanosScenario where
  runMessage msg (ThanosScenario a) = ThanosScenario <$> runMessage msg a

-- | Thanos (MTS rulebook p.16): Thanos (I) and (II), (II) and (III) for expert; the
-- Thanos and Infinity Gauntlet sets, with Black Order and Children of Thanos as the
-- modular sets. 1A has no setup of its own: the Infinity Gauntlet's Setup keyword makes
-- the stone deck, and 1B's When Revealed the rest.
thanosScenario :: ScenarioCard ThanosScenario
thanosScenario =
  scenario
    ThanosScenario
    "21111"
    ("21111", "21112")
    ["21114", "21115"]
    [EncounterSet.Thanos, EncounterSet.InfinityGauntlet]

-- The Mad Titan's Shadow: Loki

newtype LokiScenario = LokiScenario (Attrs Scenario)
  deriving anyclass IsScenario
  deriving newtype (Show, Eq, ToJSON, FromJSON)

-- | Loki (MTS rulebook p.24): five versions of Loki (I), one at random in play and the
-- others set aside; All Hail King Loki; the Loki and Infinity Gauntlet sets, with
-- Enchantress and Frost Giants the modular sets. No expert villain: the expert set only.
lokiScenario :: ScenarioCard LokiScenario
lokiScenario = scenario LokiScenario "21160" ("21160", "21160") ["21165"] [EncounterSet.Loki, EncounterSet.InfinityGauntlet]

-- | The versions of Loki, the villain deck.
lokiVersions :: [CardDef]
lokiVersions = [VillainCards.loki1, VillainCards.loki2, VillainCards.loki3, VillainCards.loki4, VillainCards.loki5]

isLokiVillain :: EncounterCard -> Bool
isLokiVillain c = cdCardType (getCardDef c) == VillainType && title (cdName (getCardDef c)) == "Loki"

-- | The first player chooses the victory condition, the Lokis in the victory display that
-- win the game (1 rookie, 2 standard, 3 expert, 4 heroic, MTS rulebook p.24), as the
-- scenario starts. Then one Loki at random is the villain, the others set aside, and
-- setup goes on as any scenario's (the Infinity Gauntlet attaching to him).
--
-- Loki defeated (1B's Forced Interrupt and his victory 1): he goes to the victory
-- display; with as many Lokis there as the victory condition, the players win;
-- otherwise a random set-aside Loki takes his place, with all that was on him, at his
-- own hit points.
instance RunMessage LokiScenario where
  runMessage msg (LokiScenario a) = LokiScenario <$> case msg of
    StartScenario -> runQueueT do
      first <- selectJust FirstPlayer
      chooseOnePrompted first "$label.chooseLokiVictoryCondition"
        [ labeled (countLabel "lokiVictoryCondition" n) do
            cardSpecific GameRef "lokiVictoryCondition" n
            cardSpecific GameRef "lokiSetup" ()
        | n <- [1 .. 4 :: Natural]
        ]
      pure a
    CardSpecific GameRef "lokiVictoryCondition" (fromJSON -> Success (n :: Natural)) ->
      pure a {Types.scenarioVictoryCondition = Just n}
    CardSpecific GameRef "lokiSetup" _ -> do
      cards <- shuffleM =<< traverse genEncounterCard lokiVersions
      case cards of
        [] -> pure a
        villain : rest ->
          runMessage StartScenario $
            a {Types.scenarioVillains = [toCardCode villain]} & setAsideCardsL <>~ map EncounterCard rest
    ResolveVillainDefeat vid -> runQueueT do
      mdef <- getsGame (`abilityCardDef` VillainRef vid)
      case mdef of
        Nothing -> pure a
        Just def -> do
          -- a card of its own: the villain id stays with the Loki in play
          addCardToVictoryDisplay =<< genEncounterCard def
          lokis <- (+ 1) . length . filter isLokiVillain <$> getVictoryDisplay
          setAside' <- shuffleM [ec | EncounterCard ec <- scenarioSetAsideCards a, isLokiVillain ec]
          case setAside' of
            next : _ | fromIntegral lokis < fromMaybe 2 (Types.scenarioVictoryCondition a) -> do
              replaceVillain vid (toCardCode next) Advanced
              pure $ a & setAsideCardsL %~ filter (/= EncounterCard next)
            _ -> a <$ gameOver Won
    _ -> runMessage msg a

-- The Mad Titan's Shadow: Hela

newtype HelaScenario = HelaScenario (Attrs Scenario)
  deriving anyclass IsScenario
  deriving newtype (Show, Eq, ToJSON, FromJSON)

instance RunMessage HelaScenario where
  runMessage msg (HelaScenario a) = HelaScenario <$> runMessage msg a

-- | Hela (MTS rulebook p.20): Hela (A), (B) for expert, one card of two sides; Odin's
-- Torment; the Hela set, with Legions of Hel and Frost Giants the modular sets. Hela
-- defeated with Odin rescued is the players' win, as any last stage's defeat.
helaScenario :: ScenarioCard HelaScenario
helaScenario = scenario HelaScenario "21136a" ("21136a", "21137a") ["21138"] [EncounterSet.Hela]

-- The Hood's modular sets begin set aside; each recruitment chooses a whole set.
newtype TheHoodScenario = TheHoodScenario (Attrs Scenario)
  deriving anyclass IsScenario
  deriving newtype (Show, Eq, ToJSON, FromJSON)

-- No particular modular sets are recommended by the product. Use the player's
-- selection as given; only the chosen sets are set aside at setup.
theHoodScenario :: ScenarioCard TheHoodScenario
theHoodScenario = scenario TheHoodScenario "24001" ("24001", "24002") ["24004", "24005", "24006"] [EncounterSet.TheHood]

instance RunMessage TheHoodScenario where
  runMessage msg (TheHoodScenario a) = TheHoodScenario <$> case msg of
    StartScenario -> runQueueT do
      let modulars = filter (`elem` EncounterSet.modularSets) (HashSet.toList $ Types.scenarioEncounterSets a)
      cards <- concatMapM gatherEncounterSet modulars
      runMessage StartScenario $ a
        { Types.scenarioEncounterSets = Types.scenarioEncounterSets a `HashSet.difference` HashSet.fromList modulars
        , Types.scenarioSetAsideCards = Types.scenarioSetAsideCards a <> map EncounterCard cards
        }
    CardSpecific GameRef "hoodRecruit" _ -> runQueueT do
      let cards = [ec | EncounterCard ec <- scenarioSetAsideCards a, cdEncounterSet (getCardDef ec) `elem` map Just EncounterSet.modularSets]
      sets <- shuffleM (nub $ map (cdEncounterSet . getCardDef) cards)
      for_ (take 1 sets) \set -> shuffleIntoEncounterDeck [ec | ec <- cards, cdEncounterSet (getCardDef ec) == set]
      pure a
    CardSpecific GameRef "hoodFoulPlay" (fromJSON -> Success (you, penalize :: Maybe MainSchemeId)) -> runQueueT do
      mv <- selectOne ActiveVillain
      let resolve = cardSpecific GameRef "resolveHoodFoulPlay" (you :: IdentityId, penalize)
      maybe resolve (\vid -> specialAbility you vid resolve) mv
      pure a
    CardSpecific GameRef "resolveHoodFoulPlay" (fromJSON -> Success (you, penalize :: Maybe MainSchemeId)) -> runQueueT do
      mv <- selectOne ActiveVillain
      stage <- maybe (pure 1) getVillainStage mv
      discardTopOfEncounterDeckThen (if stage == 1 then 1 else 2) \cards -> do
        let eligible = filter ((/= Just EncounterSet.TheHood) . cdEncounterSet . getCardDef) cards
            dealt = if stage == 3 then eligible else take 1 eligible
        traverse_ (dealFoundEncounterCard you) dealt
        when (null dealt) $ traverse_ (\mid -> cardSpecific mid "hoodFoulPlayMiss" ()) penalize
      pure a
    _ -> runMessage msg a

-- Sinister Motives

newtype SmSandmanScenario = SmSandmanScenario (Attrs Scenario)
  deriving anyclass IsScenario
  deriving newtype (Show, Eq, ToJSON, FromJSON)

smSandmanScenario :: ScenarioCard SmSandmanScenario
smSandmanScenario = scenario SmSandmanScenario "27061" ("27061", "27062") ["27064"] [EncounterSet.Sandman, EncounterSet.CityInChaos]

instance RunMessage SmSandmanScenario where
  runMessage msg (SmSandmanScenario a) = SmSandmanScenario <$> runMessage msg a

newtype SmVenomScenario = SmVenomScenario (Attrs Scenario)
  deriving anyclass IsScenario
  deriving newtype (Show, Eq, ToJSON, FromJSON)

smVenomScenario :: ScenarioCard SmVenomScenario
smVenomScenario = scenario SmVenomScenario "27073" ("27073", "27074") ["27076"] [EncounterSet.Venom, EncounterSet.SymbioticStrength]

instance RunMessage SmVenomScenario where
  runMessage msg (SmVenomScenario a) = SmVenomScenario <$> runMessage msg a

newtype SmMysterioScenario = SmMysterioScenario (Attrs Scenario)
  deriving anyclass IsScenario
  deriving newtype (Show, Eq, ToJSON, FromJSON)

smMysterioScenario :: ScenarioCard SmMysterioScenario
smMysterioScenario = scenario SmMysterioScenario "27084" ("27084", "27085") ["27087", "27088"] [EncounterSet.Mysterio, EncounterSet.PersonalNightmare]

instance RunMessage SmMysterioScenario where
  runMessage msg (SmMysterioScenario a) = SmMysterioScenario <$> runMessage msg a

newtype SmSinisterSixScenario = SmSinisterSixScenario (Attrs Scenario)
  deriving anyclass IsScenario
  deriving newtype (Show, Eq, ToJSON, FromJSON)

smSinisterSixScenario :: ScenarioCard SmSinisterSixScenario
smSinisterSixScenario = scenario SmSinisterSixScenario "27100" ("27094", "27094") ["27100", "27101"] [EncounterSet.SinisterSix]

-- | Entered villains are randomized once from the stored game generator. Defeated
-- villains return to the set-aside pool rather than ending the game.
instance RunMessage SmSinisterSixScenario where
  runMessage msg (SmSinisterSixScenario a) = SmSinisterSixScenario <$> case msg of
    StartScenario -> runQueueT do
      count <- getStartingPlayerCount
      shuffled <- shuffleM Six.sixCodes
      let (chosen, others) = splitAt (count + 1) shuffled
      aside <- traverse (genEncounterCard . (\code -> fromMaybe (error "missing Six villain") $ HashMap.lookup code VillainCards.allVillainsMap)) others
      started <- runMessage StartScenario a
        { Types.scenarioVillains = chosen
        , Types.scenarioSetAsideCards = Types.scenarioSetAsideCards a <> map EncounterCard aside
        }
      cardSpecific GameRef "sinisterSixInitialActive" ()
      pure started
    CardSpecific GameRef "sinisterSixInitialActive" _ -> runQueueT do
      present <- Six.sixVillains
      for_ (take 1 present) (setActiveVillain . fst)
      pure a
    ResolveVillainDefeat vid -> runQueueT do
      present <- Six.sixVillains
      current <- selectOne ActiveVillain
      let others = filter ((/= vid) . fst) present
          amount = if null others then 7 else 4
      schemes <- selectList AnySideScheme
      legal <- filterM (\sid -> do
        modifiers <- getModifiers (SideSchemeRef sid)
        threat <- sideSchemeThreatOf sid
        pure $ threat > 0 && ThreatCannotBeRemoved `notElem` modifiers) schemes
      first <- selectJust FirstPlayer
      unless (null legal) $ chooseOne first
        [ targeting (SideSchemeRef sid) (removeThreat first GameRef amount $ SchemeWithId $ SchemeSideSchemeId sid)
        | sid <- legal
        ]
      def <- getsGame (`abilityCardDef` VillainRef vid)
      for_ def \card -> cardSpecific GameRef "sinisterSixSetAsideDefeated" (vid, cdCardCode card, current == Just vid)
      pure a
    CardSpecific GameRef "sinisterSixSetAsideDefeated" (fromJSON -> Success (vid, code, wasActive)) -> runQueueT do
      let def = fromMaybe (error "missing defeated Six villain") $ HashMap.lookup code VillainCards.allVillainsMap
      card <- genEncounterCard def
      removeFromPlay (VillainRef vid)
      when wasActive $ cardSpecific GameRef "sinisterSixNextAfterDefeat" (code :: CardCode)
      pure a {Types.scenarioSetAsideCards = Types.scenarioSetAsideCards a <> [EncounterCard card]}
    CardSpecific GameRef "sinisterSixNextAfterDefeat" (fromJSON -> Success code) -> runQueueT do
      moveSixCounterAfter (code :: CardCode)
      pure a
    CardSpecific GameRef "sinisterSixNextActive" _ -> runQueueT do
      current <- selectOne ActiveVillain
      def <- maybe (pure Nothing) (\vid -> getsGame (`abilityCardDef` VillainRef vid)) current
      maybe (Six.sixVillains >>= traverse_ (setActiveVillain . fst) . take 1) (moveSixCounterAfter . cdCardCode) def
      pure a
    CardSpecific GameRef "sinisterSixAmbush" (fromJSON -> Success callback) -> runQueueT do
      you <- getYou
      scheme <- selectJust EncounterMainScheme
      specialAbility you scheme $ cardSpecific GameRef "resolveSinisterSixAmbush" (callback :: Maybe (Target, Text, Value))
      pure a
    CardSpecific GameRef "resolveSinisterSixAmbush" (fromJSON -> Success callback) -> runQueueT do
      let available = [card | EncounterCard card <- Types.scenarioSetAsideCards a, toCardCode card `elem` Six.sixCodes]
      shuffled <- shuffleM available
      case shuffled of
        [] -> do
          sixCallback (callback :: Maybe (Target, Text, Value)) Nothing
          pure a
        card : _ -> do
          let code = toCardCode card
          addVillain code
          cardSpecific GameRef "sinisterSixAfterAmbush" (code, True, callback :: Maybe (Target, Text, Value))
          pure a {Types.scenarioSetAsideCards = filter (/= EncounterCard card) $ Types.scenarioSetAsideCards a}
    CardSpecific GameRef "sinisterSixStageTwo" (fromJSON -> Success callback) -> runQueueT do
      let available = [card | EncounterCard card <- Types.scenarioSetAsideCards a, toCardCode card `elem` Six.sixCodes]
      shuffled <- shuffleM available
      case shuffled of
        [] -> sixCallback (callback :: Maybe (Target, Text, Value)) Nothing >> pure a
        card : _ -> do
          addVillain (toCardCode card)
          cardSpecific GameRef "sinisterSixAfterAmbush" (toCardCode card, False, callback :: Maybe (Target, Text, Value))
          pure a {Types.scenarioSetAsideCards = filter (/= EncounterCard card) $ Types.scenarioSetAsideCards a}
    CardSpecific GameRef "sinisterSixAfterAmbush" (fromJSON -> Success (code, penalty, callback)) -> runQueueT do
      present <- Six.sixVillains
      let entered = fst <$> find ((== code) . snd) present
      traverse_ setActiveVillain entered
      when (penalty && Types.scenarioDifficulty a == Expert && isJust entered) do
        lights <- (<>) <$> selectList (SideSchemeIs SideSchemes.smLightAtTheEndA) <*> selectList (SideSchemeIs SideSchemes.smLightAtTheEndB)
        for_ lights (`placeThreat` 2)
      sixCallback (callback :: Maybe (Target, Text, Value)) entered
      pure a
    CardSpecific GameRef "sinisterSixPutVillain" (fromJSON -> Success code) -> runQueueT do
      let available = [card | EncounterCard card <- Types.scenarioSetAsideCards a, toCardCode card == (code :: CardCode)]
      for_ (take 1 available) (addVillain . toCardCode)
      pure a {Types.scenarioSetAsideCards = filter (`notElem` map EncounterCard available) $ Types.scenarioSetAsideCards a}
    CardSpecific GameRef "sinisterSixActivateWithBoost" (fromJSON -> Success (you, n)) -> runQueueT do
      present <- Six.sixVillains
      current <- selectOne ActiveVillain
      case current <|> (fst <$> headMay present) of
        Just vid -> do
          setActiveVillain vid
          villainStep vid (VillainAttacksWithAdditionalBoostCards you n)
        Nothing -> Six.ambushThen GameRef "sinisterSixActivateWithBoostAfterAmbush" (you :: IdentityId, n :: Natural)
      pure a
    CardSpecific GameRef "sinisterSixActivateWithBoostAfterAmbush" (fromJSON -> Success (entered, value)) -> runQueueT do
      case (entered :: Maybe VillainId, fromJSON value) of
        (Just vid, Success (you, n)) -> do
          villainStep vid (VillainAttacksWithAdditionalBoostCards you n)
        _ -> pure ()
      pure a
    CardSpecific GameRef "sinisterSixActivate" (fromJSON -> Success (you, hero)) -> runQueueT do
      present <- Six.sixVillains
      current <- selectOne ActiveVillain
      case current <|> (fst <$> headMay present) of
        Just vid -> do
          setActiveVillain vid
          if hero then enemyAttacks vid you else villainSchemesAgainst vid you
        Nothing -> Six.ambushThen GameRef "sinisterSixActivateAfterAmbush" (you :: IdentityId, hero :: Bool)
      pure a
    CardSpecific GameRef "sinisterSixActivateAfterAmbush" (fromJSON -> Success (entered, value)) -> runQueueT do
      case (entered :: Maybe VillainId, fromJSON value) of
        (Just vid, Success (you, hero)) -> if hero then enemyAttacks vid you else villainSchemesAgainst vid you
        _ -> pure ()
      pure a
    _ -> runMessage msg a

-- | The next surviving activation order, wrapping at six; no other villain means
-- the counter remains on the single villain (or is set aside when none remain).
moveSixCounterAfter :: (ReverseQueue m) => CardCode -> m ()
moveSixCounterAfter code = do
  present <- Six.sixVillains
  let after = filter ((> Six.activationOrder code) . Six.activationOrder . snd) present
      chosen = headMay (after <> present)
  for_ chosen (setActiveVillain . fst)

sixCallback :: (ReverseQueue m) => Maybe (Target, Text, Value) -> Maybe VillainId -> m ()
sixCallback callback entered = for_ callback \(target, key, value) -> cardSpecific target key (entered, value)

newtype SmVenomGoblinScenario = SmVenomGoblinScenario (Attrs Scenario)
  deriving anyclass IsScenario
  deriving newtype (Show, Eq, ToJSON, FromJSON)

smVenomGoblinScenario :: ScenarioCard SmVenomGoblinScenario
smVenomGoblinScenario = scenario SmVenomGoblinScenario "27113" ("27113", "27114") ["27117", "27118", "27119"] [EncounterSet.VenomGoblin, EncounterSet.SymbioticStrength]

instance RunMessage SmVenomGoblinScenario where
  runMessage msg (SmVenomGoblinScenario a) = SmVenomGoblinScenario <$> case msg of
    StartScenario -> runQueueT do
      started <- runMessage StartScenario a {Types.scenarioMainSchemes = ["27117"]}
      addSetupMainScheme "27118"
      addSetupMainScheme "27119"
      cardSpecific GameRef "goblinSetup" ()
      sky <- genEncounterCard Environments.smSkiesOverNewYorkB
      pure $ started & setAsideCardsL <>~ [EncounterCard sky]
    CardSpecific GameRef "goblinSetup" _ -> runQueueT do
      schemes <- selectList MainScheme
      for_ schemes $ \case
        SchemeMainSchemeId sid -> do
          def <- getsGame (`abilityCardDef` MainSchemeRef sid)
          when (fmap toCardCode def == Just "27118") $ mainSchemeStep sid (SetMainSchemeGlider True)
        _ -> pure ()
      pure a
    CardSpecific GameRef "goblinFlip" (fromJSON -> Success (sid, def, tokens)) -> runQueueT do
      -- Flip preserves the physical card's id; tokens migrate after it changes type.
      let card = MkEncounterCard (CardId $ unMainSchemeId sid) def
      removeFromPlay (sid :: MainSchemeId)
      you <- selectJust FirstPlayer
      putEncounterCardIntoPlay you card
      cardSpecific GameRef "goblinMigrate" (you, tokens :: Natural)
      pure a
    CardSpecific GameRef "goblinMigrate" (fromJSON -> Success (you, tokens)) -> runQueueT do
      Goblin.chooseGliderScheme False you "goblinMigrationSelected"
      cardSpecific GameRef "goblinMigrationTokens" (tokens :: Natural)
      pure a
    CardSpecific GameRef "goblinMigrationSelected" _ -> pure a
    CardSpecific GameRef "goblinMigrationTokens" (fromJSON -> Success tokens) -> runQueueT do
      gliders <- getGliderSchemes
      for_ gliders $ \sid -> mainSchemeStep sid (AddMainSchemeAcceleration tokens)
      infested <- Goblin.symbioteEnvironmentCount
      when (infested >= 2) $ gameOver Lost
      pure a
    CardSpecific GameRef "goblinBerserkerBoost" (fromJSON -> Success (_you :: IdentityId, sid :: MainSchemeId)) -> runQueueT do
      placeThreat sid 1
      pure a
    CardSpecific GameRef "goblinSpecialSelected" (fromJSON -> Success (you, sid)) -> runQueueT do
      Goblin.resolveGoblinSpecial you sid
      pure a
    _ -> runMessage msg a
