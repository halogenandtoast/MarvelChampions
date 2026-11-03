-- | A complete Civil War scenario, either one cooperative board or two teams
-- exchanging independently constructed scenarios. Setup decisions use the same
-- card handlers as play; the scenario owns only deck construction and sequencing.
module Marvel.Scenario.CivilWar where

import Marvel.Prelude
import Data.HashMap.Strict qualified as HashMap
import Data.HashSet qualified as HashSet
import Data.List (nub)
import Data.Maybe (maybeToList)
import Marvel.Card
import Marvel.CivilWar.Cards qualified as C
import Marvel.CivilWar.Types qualified as CW
import Marvel.Difficulty
import Marvel.EncounterCard (gatherEncounterSets,genEncounterCard)
import Marvel.EncounterSet qualified as E
import Marvel.Entity
import Marvel.Game.Source
import Marvel.Message.Lifted
import Marvel.PlayerCard qualified as Player
import Marvel.Ref
import Marvel.Scenario.Types

newtype CivilWarScenario = CivilWarScenario (Attrs Scenario `With` CivilWarScenarioConfig)
  deriving newtype (Show,Eq,ToJSON,FromJSON)

data CivilWarScenarioConfig = CivilWarScenarioConfig
  { cwScenarioTeam :: CW.TeamScenario
  , cwScenarioCompetition :: Maybe CW.CompetitionSetup
  }
  deriving stock (Show,Eq,Generic)
  deriving anyclass (ToJSON,FromJSON)

instance IsScenario CivilWarScenario where
  toScenarioAttrs (CivilWarScenario (attrs `With` _)) = attrs
  overScenarioAttrs f (CivilWarScenario (attrs `With` config)) = CivilWarScenario (f attrs `With` config)
  scenarioCompetitionSetup (CivilWarScenario (_ `With` config)) = cwScenarioCompetition config

civilWarLeaderCodes :: [CardCode]
civilWarLeaderCodes = ["56059","56092","56137","56168"]

civilWarRecommendedModules :: [(CardCode,[E.EncounterSet])]
civilWarRecommendedModules = [(code,CW.teamScenarioModulars config) | code <- civilWarLeaderCodes,Just config <- [CW.defaultTeamScenario code]]

civilWarScenarios :: [ScenarioCard CivilWarScenario]
civilWarScenarios = map civilWarScenario civilWarLeaderCodes

civilWarScenario :: CardCode -> ScenarioCard CivilWarScenario
civilWarScenario code = ScenarioCard code $ \setup ->
  let defaults = fromMaybe (error "unknown Civil War leader") $ CW.defaultTeamScenario code
      chosen = defaults
        { CW.teamScenarioModulars = if null setup.modulars then CW.teamScenarioModulars defaults else setup.modulars
        , CW.teamScenarioMainSchemes = if null (setupMainSchemes setup) then CW.teamScenarioMainSchemes defaults else setupMainSchemes setup
        }
      base = buildScenario (scenario id code (leaderStage code Normal,leaderStage code Expert)
        (setupFaces $ CW.teamScenarioMainSchemes chosen) (maybeToList $ CW.leaderEncounterSet code)) setup
      sets = if null setup.modulars then scenarioEncounterSets base <> HashSet.fromList (CW.teamScenarioModulars defaults) else scenarioEncounterSets base
   in CivilWarScenario (base {scenarioEncounterSets = sets} `With` CivilWarScenarioConfig chosen (setupCompetition setup))

-- Stage 1 resolves its A-side setup. Stage 2 has no setup ability of its own.
setupFaces :: [CardCode] -> [CardCode]
setupFaces [] = []
setupFaces (first : rest) = flipCardCode first : rest

leaderStage :: CardCode -> Difficulty -> CardCode
leaderStage base difficulty = fromMaybe (error "missing Civil War leader stage") $ headMay
  [toCardCode def | def <- C.civilWarDefinitions,cdCardType def == VillainType,
    cdEncounterSet def == CW.leaderEncounterSet base,
    let (_,_,_,stage) = C.printedLeaderStats (toCardCode def),stage == if difficulty == Expert then 3 else 1]

-- Each physical printing contains five cards; do not gather both copies of the
-- shared Standard PvP encounter set into either team's deck.
standardPvpCards :: MonadRandom m => CW.Team -> m [EncounterCard]
standardPvpCards faction = concatMapM copies $ zip [1,1,2,1] definitions
 where
  definitions = if faction == CW.Registration
    then [C.cw56125,C.cw56126,C.cw56127,C.cw56128a]
    else [C.cw56203,C.cw56204,C.cw56205,C.cw56206a]
  copies (quantity,def) = traverse genEncounterCard $ replicate quantity def

validateCivilWarSetup :: ScenarioSetup -> Either Text ()
validateCivilWarSetup setup = case setupCompetition setup of
  Just competition -> do
    CW.validateCompetitionSetup competition
    for_ [CW.competitionRegistration competition,CW.competitionResistance competition] $ \team -> do
      unless (CW.teamScenarioLeader team `elem` civilWarLeaderCodes
        && all (`elem` E.modularSets) (CW.teamScenarioModulars team)) $ Left "error.cwContentUnavailable"
  Nothing -> do
    let code = setup.scenarioCode
    faction <- maybe (Left "error.cwLeaderFaction") Right $ CW.leaderTeam code
    defaults <- maybe (Left "error.cwLeaderFaction") Right $ CW.defaultTeamScenario code
    let chosen = if null setup.modulars then CW.teamScenarioModulars defaults else setup.modulars
        modules = filter (`notElem` [E.Standard,E.StandardII,E.StandardIII,E.ExpertSet,E.ExpertII]) chosen
        opposite = if faction == CW.Registration then CW.resistanceModulars else CW.registrationModulars
        (first,second) = if faction == CW.Registration then CW.registrationSchemes else CW.resistanceSchemes
        schemes = if null (setupMainSchemes setup) then CW.teamScenarioMainSchemes defaults else setupMainSchemes setup
    unless (length modules `elem` [3,4] && length (nub modules) == length modules) $ Left "error.cwModularCount"
    unless (all (\m -> m `notElem` opposite && m `elem` E.modularSets) modules) $ Left "error.cwModularFaction"
    unless (case schemes of [one,two] -> one `elem` first && two `elem` second; _ -> False) $ Left "error.cwMainSchemes"

instance RunMessage CivilWarScenario where
  runMessage msg (CivilWarScenario (attrs `With` config)) = runQueueT $ case msg of
    StartScenario -> case cwScenarioCompetition config of
      Nothing -> do
        updated <- runMessage msg attrs
        cardSpecific GameRef "cwCooperativeLeaderSetup" ()
        pure $ CivilWarScenario (updated `With` config)
      Just competition -> do
        live <- getCompetition
        for_ live $ \table -> do
          for_ [CW.Registration,CW.Resistance] $ \team -> do
            let opponent = CW.scenarioForTeam competition (CW.otherTeam team)
            setActivePlayer $ CW.teamFirstPlayer table team
            addVillain $ leaderStage (CW.teamScenarioLeader opponent) (scenarioDifficulty attrs)
            cardSpecific GameRef "cwPrepareBoard" team
          setActivePlayer $ CW.teamFirstPlayer table CW.Registration
        pure $ CivilWarScenario (attrs `With` config)
    CardSpecific GameRef "cwCooperativeLeaderSetup" _ -> do
      you <- getYou
      leader <- getActiveVillainIn =<< getAreaOfTarget you
      for_ leader $ \vid -> cardSpecific vid "cwInitialLeaderSetup" ()
      pure $ CivilWarScenario (attrs `With` config)
    CardSpecific GameRef "cwPrepareBoard" (fromJSON -> Success team) -> do
      live <- getCompetition
      leader <- getActiveVillainIn (CW.teamArea team)
      case (live,leader,cwScenarioCompetition config) of
        (Just _,Just vid,Just competition) -> do
          let opponent = CW.scenarioForTeam competition (CW.otherTeam team)
              family = maybeToList $ CW.leaderEncounterSet $ CW.teamScenarioLeader opponent
              modules = CW.teamScenarioModulars opponent
              extra = [E.ExpertSet | scenarioDifficulty attrs == Expert]
              pending = CW.pendingEncounterDeck team
              key = VillainDeckKey vid
              staged = fromMaybe (Pile [] []) $ HashMap.lookup pending $ scenarioDecks attrs
              early = pileDeck staged <> pileDiscard staged <> [card | team == CW.Registration,card <- scenarioEncounterDeck attrs]
          encounter <- gatherEncounterSets $ HashSet.fromList (family <> modules <> extra)
          standard <- standardPvpCards $ CW.otherTeam team
          let held = [card | EncounterCard card <- scenarioSetAsideCards attrs,HashMap.lookup (ecCardId card) (scenarioCardDecks attrs) == Just pending]
              homes = HashMap.fromList [(ecCardId card,key) | card <- early <> held]
                <> HashMap.map (\home -> if home == pending then key else home) (scenarioCardDecks attrs)
          rewards <- for (CW.leaderBasicCards $ CW.teamScenarioLeader $ CW.scenarioForTeam competition team) $ \code -> do
            cid <- getRandom
            pure $ PlayerCard $ MkPlayerCard cid (Player.lookupPlayerCard code) Nothing Nothing
          createDeck key (encounter <> standard <> early)
          for_ (take 1 $ setupFaces $ CW.teamScenarioMainSchemes opponent) addSetupMainScheme
          cardSpecific vid "cwInitialLeaderSetup" ()
          let updated = attrs
                { scenarioDecks = HashMap.delete pending $ scenarioDecks attrs
                , scenarioEncounterDeck = if team == CW.Registration then [] else scenarioEncounterDeck attrs
                , scenarioCardDecks = homes
                , scenarioSetAsideCards = scenarioSetAsideCards attrs <> rewards
                }
          pure $ CivilWarScenario (updated `With` config)
        _ -> error "Civil War board setup requires its prepared leader and teams"
    _ -> (\updated -> CivilWarScenario (updated `With` config)) <$> runMessage msg attrs
