-- | The two independent sides of a Civil War game. Teams own encounter scenarios,
-- which they give to their opponents; their leader is on the opposite game area.
module Marvel.CivilWar.Types where

import Data.List (nub)
import Marvel.Prelude
import Marvel.Area (AreaId (..))
import Marvel.Card.Code
import Marvel.EncounterSet
import Marvel.Id (IdentityId)
import Marvel.Phase (Phase (..))
import Marvel.Ref (Target)

data Team = Registration | Resistance
  deriving stock (Show, Eq, Ord, Enum, Bounded, Generic)
  deriving anyclass (ToJSON, FromJSON, Hashable)

otherTeam :: Team -> Team
otherTeam Registration = Resistance
otherTeam Resistance = Registration

teamArea :: Team -> AreaId
teamArea Registration = AreaId 0
teamArea Resistance = AreaId 1

data TeamScenario = TeamScenario
  { teamScenarioLeader :: CardCode
  , teamScenarioModulars :: [EncounterSet]
  , teamScenarioMainSchemes :: [CardCode]
  }
  deriving stock (Show, Eq, Generic)
instance ToJSON TeamScenario where toJSON = genericToJSON $ aesonOptions $ Just "teamScenario"
instance FromJSON TeamScenario where parseJSON = genericParseJSON $ aesonOptions $ Just "teamScenario"

data CompetitionSetup = CompetitionSetup
  { competitionRegistration :: TeamScenario
  , competitionResistance :: TeamScenario
  }
  deriving stock (Show, Eq, Generic)
instance ToJSON CompetitionSetup where toJSON = genericToJSON $ aesonOptions $ Just "competition"
instance FromJSON CompetitionSetup where parseJSON = genericParseJSON $ aesonOptions $ Just "competition"

scenarioForTeam :: CompetitionSetup -> Team -> TeamScenario
scenarioForTeam setup Registration = competitionRegistration setup
scenarioForTeam setup Resistance = competitionResistance setup

-- | Separate phases, rather than alternating turns between opponents.
data TeamPhase
  = RegistrationPlayerPhase
  | ResistancePlayerPhase
  | RegistrationVillainPhase
  | ResistanceVillainPhase
  deriving stock (Show, Eq, Ord, Enum, Bounded, Generic)
  deriving anyclass (ToJSON, FromJSON)

phaseTeam :: TeamPhase -> Team
phaseTeam RegistrationPlayerPhase = Registration
phaseTeam RegistrationVillainPhase = Registration
phaseTeam ResistancePlayerPhase = Resistance
phaseTeam ResistanceVillainPhase = Resistance

ordinaryPhase :: TeamPhase -> Phase
ordinaryPhase RegistrationPlayerPhase = PlayerPhase
ordinaryPhase ResistancePlayerPhase = PlayerPhase
ordinaryPhase RegistrationVillainPhase = VillainPhase
ordinaryPhase ResistanceVillainPhase = VillainPhase

nextTeamPhase :: TeamPhase -> Maybe TeamPhase
nextTeamPhase RegistrationPlayerPhase = Just ResistancePlayerPhase
nextTeamPhase ResistancePlayerPhase = Just RegistrationVillainPhase
nextTeamPhase RegistrationVillainPhase = Just ResistanceVillainPhase
nextTeamPhase ResistanceVillainPhase = Nothing

-- | Record the first result on each board, then give the other side the same
-- opportunity before declaring a winner. A defeated leader stays on the table.
data TeamOutcome = EnemyLeaderDefeated | MainSchemeCompleted | AllHeroesDefeated
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON)

data Competition = Competition
  { competitiveSetup :: CompetitionSetup
  , competitivePhase :: TeamPhase
  , competitiveRegistrationPlayers :: [IdentityId]
  , competitiveResistancePlayers :: [IdentityId]
  , competitiveRegistrationFirstPlayer :: IdentityId
  , competitiveResistanceFirstPlayer :: IdentityId
  , competitiveRegistrationOutcome :: Maybe TeamOutcome
  , competitiveResistanceOutcome :: Maybe TeamOutcome
  , competitiveEffectAreas :: [(Target, AreaId)]
  }
  deriving stock (Show, Eq, Generic)
instance ToJSON Competition where toJSON = genericToJSON $ aesonOptions $ Just "competitive"
instance FromJSON Competition where parseJSON = genericParseJSON $ aesonOptions $ Just "competitive"

teamPlayers :: Competition -> Team -> [IdentityId]
teamPlayers c Registration = competitiveRegistrationPlayers c
teamPlayers c Resistance = competitiveResistancePlayers c

teamFirstPlayer :: Competition -> Team -> IdentityId
teamFirstPlayer c Registration = competitiveRegistrationFirstPlayer c
teamFirstPlayer c Resistance = competitiveResistanceFirstPlayer c

setTeamFirstPlayer :: Team -> IdentityId -> Competition -> Competition
setTeamFirstPlayer Registration you c = c {competitiveRegistrationFirstPlayer = you}
setTeamFirstPlayer Resistance you c = c {competitiveResistanceFirstPlayer = you}

playerTeam :: Competition -> IdentityId -> Maybe Team
playerTeam c you
  | you `elem` competitiveRegistrationPlayers c = Just Registration
  | you `elem` competitiveResistancePlayers c = Just Resistance
  | otherwise = Nothing

teamOutcome :: Competition -> Team -> Maybe TeamOutcome
teamOutcome c Registration = competitiveRegistrationOutcome c
teamOutcome c Resistance = competitiveResistanceOutcome c

recordTeamOutcome :: Team -> TeamOutcome -> Competition -> Competition
recordTeamOutcome Registration result c = c {competitiveRegistrationOutcome = competitiveRegistrationOutcome c <|> Just result}
recordTeamOutcome Resistance result c = c {competitiveResistanceOutcome = competitiveResistanceOutcome c <|> Just result}

-- | Registration seats precede resistance seats in the lobby, giving each side
-- one player in 1v1 or two players in 2v2. Turn order can subsequently rotate.
newCompetition :: CompetitionSetup -> [IdentityId] -> Maybe Competition
newCompetition setup players
  | length players `elem` [2, 4] =
      let (registration, resistance) = splitAt (length players `div` 2) players
       in case (registration,resistance) of
            (firstRegistration : _,firstResistance : _) -> Just $ Competition setup RegistrationPlayerPhase registration resistance firstRegistration firstResistance Nothing Nothing []
            _ -> Nothing
  | otherwise = Nothing

leaderTeam :: CardCode -> Maybe Team
leaderTeam code
  | code `elem` ["56059", "56092", "57001"] = Just Registration
  | code `elem` ["56137", "56168", "57040"] = Just Resistance
  | otherwise = Nothing

leaderEncounterSet :: CardCode -> Maybe EncounterSet
leaderEncounterSet = \case
  "56059" -> Just CwIronMan
  "56092" -> Just CwCaptainMarvel
  "56137" -> Just CwCaptainAmerica
  "56168" -> Just CwSpiderWoman
  "57001" -> Just CwSheHulk
  "57040" -> Just CwVision
  _ -> Nothing

leaderBasicCards :: CardCode -> [CardCode]
leaderBasicCards = \case
  "56059" -> ["56129", "56130", "56131", "56132"]
  "56092" -> ["56133", "56134", "56135", "56136"]
  "56137" -> ["56207", "56208", "56209", "56210"]
  "56168" -> ["56211", "56212", "56213", "56214"]
  "57001" -> ["57032", "57033", "57034", "57035"]
  "57040" -> ["57074", "57075", "57076", "57077"]
  _ -> []

registrationModulars, resistanceModulars :: [EncounterSet]
registrationModulars =
  [CwMightyAvengers, CwTheInitiative, CwMariaHillModular, CwDangerousRecruits,
   CwCapeKiller, CwMartialLaw, CwHeroesForHire, CwPaladin,
   CwShieldOps, CwThunderboltsModular, CwTaskmasterModular, CwDeadlyDuo]
resistanceModulars =
  [CwNewAvengers, CwSecretAvengers, CwNamorModular, CwAtlanteans,
   CwSpiderManModular, CwDefenders, CwHellsKitchen, CwCloakAndDaggerModular,
   CwYoungAvengers, CwScarletTwins, CwMoonKnightModular, CwRoyalGuard]

registrationSchemes, resistanceSchemes :: ([CardCode], [CardCode])
registrationSchemes =
  (["56063b", "56096b", "56121b", "56122b", "57005b"],
   ["56064b", "56097b", "56123b", "56124b", "57006b"])
resistanceSchemes =
  (["56141b", "56172b", "56199b", "56200b", "57044b"],
   ["56142b", "56173b", "56201b", "56202b", "57045b"])

-- | Rulebook preconstructed scenarios; custom games can exchange schemes and
-- choose three or four modular sets without changing their faction.
defaultTeamScenario :: CardCode -> Maybe TeamScenario
defaultTeamScenario = \case
  "56059" -> Just $ TeamScenario "56059" [CwMightyAvengers,CwTheInitiative,CwMariaHillModular,CwDangerousRecruits] ["56063b","56064b"]
  "56092" -> Just $ TeamScenario "56092" [CwCapeKiller,CwMartialLaw,CwHeroesForHire,CwPaladin] ["56096b","56097b"]
  "56137" -> Just $ TeamScenario "56137" [CwNewAvengers,CwSecretAvengers,CwNamorModular,CwAtlanteans] ["56141b","56142b"]
  "56168" -> Just $ TeamScenario "56168" [CwSpiderManModular,CwDefenders,CwHellsKitchen,CwCloakAndDaggerModular] ["56172b","56173b"]
  "57001" -> Just $ TeamScenario "57001" [CwShieldOps,CwThunderboltsModular,CwTaskmasterModular,CwDeadlyDuo] ["57005b","57006b"]
  "57040" -> Just $ TeamScenario "57040" [CwYoungAvengers,CwScarletTwins,CwMoonKnightModular,CwRoyalGuard] ["57044b","57045b"]
  _ -> Nothing

-- | Server-side validation of the rulebook's independent scenario construction.
-- Errors are translation keys shared with the client.
validateCompetitionSetup :: CompetitionSetup -> Either Text ()
validateCompetitionSetup config = do
  validateTeamScenario Registration (competitionRegistration config)
  validateTeamScenario Resistance (competitionResistance config)

validateTeamScenario :: Team -> TeamScenario -> Either Text ()
validateTeamScenario team config = do
  unless (leaderTeam (teamScenarioLeader config) == Just team) $ Left "error.cwLeaderFaction"
  let chosen = teamScenarioModulars config
      allowed = if team == Registration then registrationModulars else resistanceModulars
      (first,second) = if team == Registration then registrationSchemes else resistanceSchemes
  unless (length chosen `elem` [3,4] && length (nub chosen) == length chosen) $ Left "error.cwModularCount"
  unless (all (`elem` allowed) chosen) $ Left "error.cwModularFaction"
  unless (case teamScenarioMainSchemes config of [one,two] -> one `elem` first && two `elem` second; _ -> False) $
    Left "error.cwMainSchemes"
