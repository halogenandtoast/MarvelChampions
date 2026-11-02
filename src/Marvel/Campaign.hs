{-# LANGUAGE DuplicateRecordFields #-}
-- | A campaign: scenarios played one after another, what each game leaves behind kept in
-- a campaign log ("docs/rules/campaign-rise-of-red-skull.md"). The campaign lives outside
-- the game: the server keeps it between games, and each game of it is dealt with a copy
-- to read as it is set up and to write in as it is won.
module Marvel.Campaign where

import Marvel.Prelude

import Data.List (find, nub)
import Data.List qualified as List
import Control.Monad (mfilter)
import Data.Maybe (maybeToList)
import Marvel.Card.Code
import Marvel.Aspect (Aspect (..))

data CampaignMode = StandardCampaign | ExpertCampaign
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON)

-- | A choice a victory leaves a player, asked as it is won; of a player not in the
-- game then, as the next game is set up.
data CampaignChoice
  = -- | Crossbones: one of the Tech upgrades of the Hydra Campaign set
    ChooseTechUpgrade
  | -- | Absorbing Man: may choose one of the "Basic" Condition upgrades
    ChooseBasicUpgrade
  | -- | Zola: may replace the "Basic" Condition upgrade with its "Improved" side
    MayImproveUpgrade
  | ChooseSinisterTech
  | ChooseSinisterAspectAdvantage
  | ChooseSinisterPlanningAhead
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON)

-- | A column of the log's Player Information.
data CampaignPlayer = CampaignPlayer
  { identity :: CardCode
  -- ^ the hero face's code: a player keeps their identity all campaign
  , number :: Int
  -- ^ 1 to 4: the expert campaign set they take obligations from
  , hitPoints :: Maybe Natural
  -- ^ remaining hit points, recorded after each win of an expert campaign
  , obligations :: [CardCode]
  , techUpgrade :: Maybe CardCode
  , conditionUpgrade :: Maybe CardCode
  -- ^ the "Basic" face ("04159a") or, once improved, the "Improved" one ("04159b")
  , rescuedAllies :: [CardCode]
  , pending :: [CampaignChoice]
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON)

data CampaignLog = CampaignLog
  { players :: [CampaignPlayer]
  , experimentalWeapons :: [CardCode]
  -- ^ Scenario 1: Experimental Weapons added to the encounter deck
  , delayCounters :: Maybe Natural
  -- ^ Scenario 2: delay counters on the main scheme
  , engagedWithMinions :: [CardCode]
  -- ^ Scenario 4: the identities of the players engaged with minions
  , alliesRemoved :: [CardCode]
  -- ^ Scenario 4: allies removed from the campaign
  , galaxy :: GalaxyLog
  -- ^ The Galaxy's Most Wanted's log ("docs/rules/campaign-galaxys-most-wanted.md")
  , titan :: TitanLog
  -- ^ The Mad Titan's Shadow's log ("docs/rules/campaign-mad-titans-shadow.md")
  , mutantGenesis :: MutantGenesisLog
  , apocalypse :: ApocalypseLog
  , shield :: ShieldLog
  , nextEvolution :: NextEvolutionLog
  , sinister :: SinisterLog
  -- ^ Sinister Motives: reputation and the campaign log (MC27 pp.22–23)
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON)

-- | A player's column of The Galaxy's Most Wanted log, besides their identity and
-- remaining hit points ('CampaignPlayer').
data GalaxyPlayer = GalaxyPlayer
  { identity :: CardCode
  , units :: Natural
  -- ^ Unspent Units
  , marketCards :: [CardCode]
  -- ^ Market Cards in Player's Deck
  , collection :: [CardCode]
  -- ^ Cards in The Collection: the player's cards there as Infiltrate the Museum is won
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON)

data GalaxyLog = GalaxyLog
  { galaxyPlayers :: [GalaxyPlayer]
  , galacticArtifacts :: [CardCode]
  -- ^ Scenario 3: Galactic Artifacts side schemes in the victory display
  , powerStoneControl :: Maybe CardCode
  -- ^ Scenario 4: the identity the Power Stone is attached to
  , headhunterDefeated :: [Int]
  -- ^ Scenarios 1 to 4: those Badoon Headhunter was in the victory display of
  , evasionCounters :: Maybe Natural
  -- ^ Scenario 4: evasion counters on Nebula's Ship
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON)

-- | A log saved before The Galaxy's Most Wanted has its section empty.
instance FromJSON GalaxyLog where
  parseJSON = genericParseJSON defaultOptions
  omittedField = Just noGalaxyLog

noGalaxyLog :: GalaxyLog
noGalaxyLog = GalaxyLog [] [] Nothing [] Nothing

-- | The Mad Titan's Shadow's log: its nine checkboxes, the campaign pool and what the
-- scenarios record (rulebook p.28), each player's identity and remaining hit points being
-- 'CampaignPlayer''s.
data TitanMark
  = -- | Scenario 1
    CosmoInPool
  | SecurityBreachInPool
  | -- | Scenario 2
    ShawarmaInPool
  | BlackSwanInPool
  | AvengersTowerDamaged
  | -- | Scenario 3
    SystemShockInPool
  | InfinityStones1BCompleted
  | -- | Scenario 4
    NornStoneInPool
  | OdinInPool
  deriving stock (Show, Eq, Generic, Enum, Bounded)
  deriving anyclass (ToJSON, FromJSON)

newtype TitanLog = TitanLog {marks :: [TitanMark]}
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON)

-- | A log saved before The Mad Titan's Shadow has its section empty.
instance FromJSON TitanLog where
  parseJSON = genericParseJSON defaultOptions
  omittedField = Just (TitanLog [])

-- | Whether the log has the box checked.
hasMark :: TitanMark -> Campaign -> Bool
hasMark m c = m `elem` c.campaignLog.titan.marks

-- | A player's three reputation-track rewards. Aspect Advantage keeps its number of
-- copies as well as its title's printing: the reward grants that title's maximum
-- allowed copies, which can differ from the normal three (MC27 p.22).
data SinisterPlayer = SinisterPlayer
  { identity :: CardCode
  , shieldTech :: Maybe CardCode
  , aspectAdvantage :: Maybe (CardCode, Natural)
  , planningAhead :: Maybe CardCode
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON)

-- | The nodes marked and the titles written in the Sinister Motives log. Reputation
-- is signed because the victory display can include Victory -1; applying the scoring
-- instructions belongs to the campaign steps, not JSON or record storage.
data SinisterLog = SinisterLog
  { sinisterPlayers :: [SinisterPlayer]
  , reputation :: Int
  , communityService :: [CardCode]
  , osbornTech :: [CardCode]
  , wakingNightmare :: Maybe Natural
  , lastOnesStanding :: [CardCode]
  , finalReputation :: Maybe Int
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON)

instance FromJSON SinisterLog where
  parseJSON = withObject "SinisterLog" \o ->
    SinisterLog
      <$> o .:? "sinisterPlayers" .!= []
      <*> o .:? "reputation" .!= 0
      <*> o .:? "communityService" .!= []
      <*> o .:? "osbornTech" .!= []
      <*> o .:? "wakingNightmare"
      <*> o .:? "lastOnesStanding" .!= []
      <*> o .:? "finalReputation"
  omittedField = Just noSinisterLog

noSinisterLog :: SinisterLog
noSinisterLog = SinisterLog [] 0 [] [] Nothing [] Nothing

-- | Chosen fronts retain the scenario number so a lost game can retry its choice.
-- Earned environments are recorded only at victory. Cards granted by their actions
-- are temporary additions to the current game.
-- | Mission results persist even when the scenario has to be replayed. A selected
-- mission is recorded by scenario number so retries cannot reroll it.
-- | Hidden evidence is stored with the deterministic game, and redacted by the API.
data ShieldLog = ShieldLog
  { hiddenEvidence :: [CardCode]
  , evidenceEnvelope :: [CardCode]
  , earnedEvidence :: [CardCode]
  , boardSecrets :: [(CardCode, Natural)]
  , betrayedBoard :: [CardCode]
  , widowRemaining :: Natural
  , rescuedCaptives :: Natural
  , adaptoidEnvironments :: [CardCode]
  , survivingThunderbolts :: [CardCode]
  , accusation :: [CardCode]
  , evidenceVictories :: Natural
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass ToJSON

noShieldLog :: ShieldLog
noShieldLog = ShieldLog [] [] [] [] [] 0 0 [] [] [] 0

instance FromJSON ShieldLog where
  parseJSON = withObject "ShieldLog" \o -> ShieldLog
    <$> o .:? "hiddenEvidence" .!= []
    <*> o .:? "evidenceEnvelope" .!= []
    <*> o .:? "earnedEvidence" .!= []
    <*> o .:? "boardSecrets" .!= []
    <*> o .:? "betrayedBoard" .!= []
    <*> o .:? "widowRemaining" .!= 0
    <*> o .:? "rescuedCaptives" .!= 0
    <*> o .:? "adaptoidEnvironments" .!= []
    <*> o .:? "survivingThunderbolts" .!= []
    <*> o .:? "accusation" .!= []
    <*> o .:? "evidenceVictories" .!= 0
  omittedField = Just noShieldLog

redactCampaign :: Campaign -> Campaign
redactCampaign c = c {campaignLog = c.campaignLog {shield = c.campaignLog.shield {hiddenEvidence = [], evidenceEnvelope = []}}}

data ApocalypseLog = ApocalypseLog
  { missions :: [(Int, CardCode)]
  , successfulMissions :: [CardCode]
  , defeatedOverseers :: [CardCode]
  , apocalypseRewards :: [(CardCode, [CardCode])]
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass ToJSON

instance FromJSON ApocalypseLog where
  parseJSON = withObject "ApocalypseLog" \o -> ApocalypseLog
    <$> o .:? "missions" .!= []
    <*> o .:? "successfulMissions" .!= []
    <*> o .:? "defeatedOverseers" .!= []
    <*> o .:? "apocalypseRewards" .!= []
  omittedField = Just noApocalypseLog

noApocalypseLog :: ApocalypseLog
noApocalypseLog = ApocalypseLog [] [] [] []

apocalypseMissionCards :: [CardCode]
apocalypseMissionCards = ["45166a", "45167a", "45168a", "45169a"]

apocalypseOverseers :: [CardCode]
apocalypseOverseers = ["45179a", "45180a", "45181a", "45182a", "45183a"]

apocalypseRewardsFor :: CardCode -> Campaign -> [CardCode]
apocalypseRewardsFor ident c = fromMaybe [] (List.lookup (toHeroCardCode ident) c.campaignLog.apocalypse.apocalypseRewards)

data NextEvolutionLog = NextEvolutionLog
  { chosenSchemes :: [(Int, CardCode)]
  , earnedEnvironments :: [CardCode]
  , maraudersDefeated :: [CardCode]
  , morlocksSaved :: Natural
  , hopeDamage :: [(Int, Natural)]
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass ToJSON

instance FromJSON NextEvolutionLog where
  parseJSON = withObject "NextEvolutionLog" \o ->
    NextEvolutionLog <$> o .:? "chosenSchemes" .!= []
      <*> o .:? "earnedEnvironments" .!= []
      <*> o .:? "maraudersDefeated" .!= []
      <*> o .:? "morlocksSaved" .!= 0
      <*> o .:? "hopeDamage" .!= []
  omittedField = Just noNextEvolutionLog

noNextEvolutionLog :: NextEvolutionLog
noNextEvolutionLog = NextEvolutionLog [] [] [] 0 []

nextEvolutionSchemes :: [(CardCode, CardCode, CardCode)]
nextEvolutionSchemes =
  [("40190a", "40190b", "40199"), ("40191a", "40191b", "40201")
  ,("40192a", "40192b", "40203"), ("40193a", "40193b", "40200")
  ,("40194a", "40194b", "40198"), ("40195a", "40195b", "40202")]

-- | A loss keeps the current scenario's chosen side scheme, but earns no reward.
-- It must be chosen again on replay even if its reverse was earned in the lost game.
retainNextEvolutionRetryChoice :: Campaign -> Campaign -> Campaign
retainNextEvolutionRetryChoice previous current
  | previous.kind == AgentsOfShieldCampaign, current.kind == AgentsOfShieldCampaign =
      let before = previous.campaignLog.shield
          now = current.campaignLog.shield
          retained = if scenarioNumber previous == 5 && previous.mode == StandardCampaign
            then before {hiddenEvidence = [], evidenceEnvelope = [], earnedEvidence = [], accusation = []}
            else if null before.hiddenEvidence then before {hiddenEvidence = now.hiddenEvidence, evidenceEnvelope = now.evidenceEnvelope} else before
      in previous {campaignLog = previous.campaignLog {shield = retained}}
  | previous.kind == AgeOfApocalypseCampaign, current.kind == AgeOfApocalypseCampaign =
      let n = previous.completed + 1
          chosen = filter ((== n) . fst) current.campaignLog.apocalypse.missions
      in previous {campaignLog = previous.campaignLog
        {apocalypse = previous.campaignLog.apocalypse
          {missions = filter ((/= n) . fst) previous.campaignLog.apocalypse.missions <> chosen}}}
  | previous.kind == NextEvolution, current.kind == NextEvolution
  , previous.completed == current.completed =
      let n = scenarioNumber previous
          choices = filter ((== n) . fst) current.campaignLog.nextEvolution.chosenSchemes
       in previous {campaignLog = previous.campaignLog
            {nextEvolution = previous.campaignLog.nextEvolution
              {chosenSchemes = filter ((/= n) . fst) previous.campaignLog.nextEvolution.chosenSchemes <> choices}}}
  | otherwise = previous

-- | The four team roles, fixed and distinct for a Mutant Genesis campaign.
data MutantRole = Brawler | Commander | Defender | Peacekeeper
  deriving stock (Show, Eq, Generic, Enum, Bounded)
  deriving anyclass (ToJSON, FromJSON)

data MutantPlayer = MutantPlayer {identity :: CardCode, role :: Maybe MutantRole}
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON)

data MutantGenesisLog = MutantGenesisLog
  { mutantPlayers :: [MutantPlayer]
  , defeatedSchemes :: [CardCode]
  , futurePastRemoved :: [CardCode]
  , futurePastActive :: [CardCode]
  , futurePastDeck :: [CardCode]
  , roleUpgradesRemoved :: [CardCode]
  , roleUpgradesInPlay :: [CardCode]
  , jubileeInPlay :: Bool
  , captiveAllies :: [CardCode]
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass ToJSON

instance FromJSON MutantGenesisLog where
  parseJSON = genericParseJSON defaultOptions
  omittedField = Just noMutantGenesisLog

noMutantGenesisLog :: MutantGenesisLog
noMutantGenesisLog = MutantGenesisLog [] [] [] [] [] [] [] False []

mutantPlayer :: CardCode -> Campaign -> Maybe MutantPlayer
mutantPlayer ident c = find ((== toHeroCardCode ident) . (.identity)) c.campaignLog.mutantGenesis.mutantPlayers

mutantRoleCards :: MutantRole -> [CardCode]
mutantRoleCards r = map (CardCode . tshow) [start .. start + 4]
 where
  start = case r of { Brawler -> 32176; Commander -> 32181; Defender -> 32186; Peacekeeper -> 32191 } :: Int

mutantRoleAspects :: MutantRole -> [Aspect]
mutantRoleAspects = \case
  Brawler -> [Aggression, Protection]
  Commander -> [Aggression, Leadership]
  Defender -> [Justice, Protection]
  Peacekeeper -> [Justice, Leadership]

-- | Which box's campaign: a campaign saved before there were two is The Rise of Red Skull.
data CampaignKind = RiseOfRedSkull | GalaxysMostWanted | MadTitansShadow | SinisterMotives | MutantGenesis | NextEvolution | AgeOfApocalypseCampaign | AgentsOfShieldCampaign
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON)

instance FromJSON CampaignKind where
  parseJSON = genericParseJSON defaultOptions
  omittedField = Just RiseOfRedSkull

data Campaign = Campaign
  { mode :: CampaignMode
  , kind :: CampaignKind
  , scenarios :: [CardCode]
  -- ^ in the order they are played, by the code each is set up with
  , completed :: Int
  -- ^ how many have been won: the next is the one after them
  , lost :: Bool
  -- ^ an expert campaign's last scenario lost: Red Skull conquers the world
  , campaignLog :: CampaignLog
  , campaignName :: Maybe Text
  -- ^ what its players call it
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON)

-- | Which part of a scenario's Campaign Instructions is being followed: the choices the
-- last victory left, before setup cards are put into play; its Setup bullets; its
-- Victory bullets. A scenario answers 'Marvel.Message.CampaignStep' to add its own.
data CampaignStepKind
  = CampaignChoices
  | CampaignSetup
  | CampaignVictory
  | -- | a later part of the Setup, seeing the game as its earlier parts left it
    CampaignSetupThen Text
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON)

-- | What a scenario writes in the log, the player named by their identity's code.
data CampaignRecord
  = RecordShieldLog ShieldLog
  | RecordHitPoints CardCode Natural
  | RecordExperimentalWeapons [CardCode]
  | RecordDelayCounters Natural
  | RecordEngagedWithMinions CardCode
  | RecordAlliesRemoved [CardCode]
  | RecordRescuedAllies CardCode [CardCode]
  | RecordTechUpgrade CardCode CardCode
  | RecordConditionUpgrade CardCode CardCode
  | RecordObligation CardCode CardCode
  | -- | "remove it from the campaign log": the player's card, crossed out
    RemoveFromCampaignLog CardCode CardCode
  | RecordChoice CardCode CampaignChoice
  | ResolvedChoice CardCode CampaignChoice
  | -- | the scenario is won: the next one is played next
    RecordScenarioWon
  | -- | The Galaxy's Most Wanted: units added to the player's Unspent Units
    RecordUnits CardCode Natural
  | -- | units subtracted, to heal (expert)
    SpendUnits CardCode Natural
  | -- | a card of The Market bought with its unit cost
    BuyMarketCard CardCode CardCode
  | RecordCollection CardCode [CardCode]
  | RecordGalacticArtifacts [CardCode]
  | RecordPowerStoneControl CardCode
  | RecordHeadhunterDefeated Int
  | RecordEvasionCounters Natural
  | -- | The Mad Titan's Shadow: a box of its log checked
    RecordTitanMark TitanMark
  | -- | Sinister Motives: reputation earned by the group, added to its current score
    RecordReputation Int
  | RecordCommunityService [CardCode]
  | RecordOsbornTech CardCode
  | RecordShieldTech CardCode CardCode
  | RecordAspectAdvantage CardCode CardCode Natural
  | RecordPlanningAhead CardCode CardCode
  | RecordWakingNightmare Natural
  | RecordLastOnesStanding [CardCode]
  | RecordFinalReputation
  | RecordApocalypseMission Int CardCode
  | RecordApocalypseMissionSuccess CardCode
  | RecordApocalypseOverseer CardCode
  | RecordApocalypseReward CardCode CardCode
  | RemoveApocalypseReward CardCode
  | RecordCampaignLost
  | RecordNextEvolutionScheme Int CardCode
  | RecordNextEvolutionEnvironment CardCode
  | RecordNextEvolutionMarauders [CardCode]
  | RecordNextEvolutionMorlocks Natural
  | RecordNextEvolutionHopeDamage Int Natural
  | RecordMutantRole CardCode MutantRole
  | RecordMutantSchemeDefeated CardCode
  | RecordFuturePast [CardCode] [CardCode]
  | RecordFuturePastDeck [CardCode]
  | RecordMutantRoleUpgrade CardCode
  | RemoveMutantRoleUpgrade CardCode
  | RecordMutantJubilee Bool
  | RecordMutantCaptives [CardCode]
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON)

-- | The Rise of Red Skull, by its villains: Crossbones, Absorbing Man, Taskmaster, Zola,
-- Red Skull.
riseOfRedSkull :: [CardCode]
riseOfRedSkull = ["04058", "04076", "04093", "04109", "04125"]

-- | The Galaxy's Most Wanted, by its villains: Drang, Collector (Infiltrate the Museum),
-- Collector (A1, Escape the Museum), Nebula, Ronan the Accuser.
galaxysMostWanted :: [CardCode]
galaxysMostWanted = ["16058", "16070", "16080a", "16088", "16103"]

-- | The Mad Titan's Shadow: Ebony Maw, Tower Defense, Thanos, Hela, Loki.
madTitansShadow :: [CardCode]
madTitansShadow = ["21071", "21098", "21111", "21136a", "21160"]

-- | Sinister Motives: Sandman, Venom, Mysterio, the Sinister Six and Venom Goblin.
-- The Six is set up by its main scheme, like Tower Defense.
sinisterMotives :: [CardCode]
sinisterMotives = ["27061", "27073", "27084", "27100", "27113"]

campaignScenarios :: CampaignKind -> [CardCode]
campaignScenarios = \case
  RiseOfRedSkull -> riseOfRedSkull
  GalaxysMostWanted -> galaxysMostWanted
  MadTitansShadow -> madTitansShadow
  SinisterMotives -> sinisterMotives
  MutantGenesis -> ["32060", "32084", "32109", "32121a", "32138"]
  NextEvolution -> ["40077", "40103", "40118", "40136", "40163"]
  AgeOfApocalypseCampaign -> ["45059", "45081a", "45101b", "45118", "45184a"]
  AgentsOfShieldCampaign -> ["50064", "50086a", "50103a", "50129a", "50165a"]

-- | The cards of The Market and their Unit Cost (rulebook p.5), four at each cost.
marketUnitCosts :: [(CardCode, Natural)]
marketUnitCosts =
  [ (CardCode (tshow (16150 + 4 * (cost - 1) + i)), fromIntegral cost)
  | cost <- [1 .. 7 :: Int]
  , i <- [0 .. 3]
  ]

-- | The Market's cards the group has not bought: one copy of each per campaign.
marketLeft :: Campaign -> [(CardCode, Natural)]
marketLeft c = [m | m@(code, _) <- marketUnitCosts, code `notElem` concatMap (.marketCards) c.campaignLog.galaxy.galaxyPlayers]

-- | The player can buy the card of The Market: unbought, its unit cost no more than their
-- Unspent Units.
canBuy :: CardCode -> CardCode -> Campaign -> Bool
canBuy ident code c =
  c.kind == GalaxysMostWanted && case (List.lookup code (marketLeft c), galaxyPlayer ident c) of
    (Just cost, Just p) -> cost <= p.units
    _ -> False

galaxyPlayer :: CardCode -> Campaign -> Maybe GalaxyPlayer
galaxyPlayer ident c = find ((== toHeroCardCode ident) . (.identity)) c.campaignLog.galaxy.galaxyPlayers

-- | The Hydra Campaign set's Tech upgrades: Adrenal Stims, Tactical Scanner, Emergency
-- Teleporter, Laser Cannon.
techUpgrades :: [CardCode]
techUpgrades = ["04155", "04156", "04157", "04158"]

-- | Its "Basic" Condition upgrades, each with its "Improved" side as the "b" code.
basicUpgrades :: [CardCode]
basicUpgrades = ["04159a", "04160a", "04161a", "04162a"]

-- | The obligations of an Expert Campaign Set: each player has a set of their own.
expertCampaignObligations :: [CardCode]
expertCampaignObligations = ["04163", "04164", "04165", "04166"]

-- | A new Rise of Red Skull campaign for the identities, numbered in the order given.
newCampaign :: CampaignMode -> [CardCode] -> Campaign
newCampaign = newCampaignOf RiseOfRedSkull

-- | A new campaign of the box for the identities, numbered in the order given.
newCampaignOf :: CampaignKind -> CampaignMode -> [CardCode] -> Campaign
newCampaignOf kind mode identities =
  Campaign
    { mode
    , kind
    , scenarios = campaignScenarios kind
    , completed = 0
    , lost = False
    , campaignName = Nothing
    , campaignLog =
        CampaignLog
          { players = zipWith newPlayer [1 ..] (map toHeroCardCode identities)
          , experimentalWeapons = []
          , delayCounters = Nothing
          , engagedWithMinions = []
          , alliesRemoved = []
          , galaxy = noGalaxyLog {galaxyPlayers = [GalaxyPlayer (toHeroCardCode i) 0 [] [] | kind == GalaxysMostWanted, i <- identities]}
          , titan = TitanLog []
          , shield = noShieldLog
          , apocalypse = noApocalypseLog
          , nextEvolution = noNextEvolutionLog
          , mutantGenesis = noMutantGenesisLog {mutantPlayers = [MutantPlayer (toHeroCardCode i) Nothing | kind == MutantGenesis, i <- identities]}
          , sinister = noSinisterLog {sinisterPlayers = [SinisterPlayer (toHeroCardCode i) Nothing Nothing Nothing | kind == SinisterMotives, i <- identities]}
          }
    }
 where
  newPlayer n ident = CampaignPlayer ident n Nothing [] Nothing Nothing [] []

isExpertCampaign :: Campaign -> Bool
isExpertCampaign c = c.mode == ExpertCampaign

-- | The scenario to play next, none once the campaign is over.
currentScenario :: Campaign -> Maybe CardCode
currentScenario c
  | c.lost = Nothing
  | otherwise = headMay (drop c.completed c.scenarios)

-- | The place in the campaign of the scenario played next: 1 to 5.
scenarioNumber :: Campaign -> Int
scenarioNumber c = c.completed + 1

campaignWon :: Campaign -> Bool
campaignWon c = not c.lost && c.completed >= length c.scenarios

campaignPlayer :: CardCode -> Campaign -> Maybe CampaignPlayer
campaignPlayer ident c = find ((== toHeroCardCode ident) . (.identity)) c.campaignLog.players

-- | The cards the campaign has added to the player's deck.
campaignCards :: CampaignPlayer -> [CardCode]
campaignCards p = maybeToList p.techUpgrade <> maybeToList p.conditionUpgrade <> p.rescuedAllies <> p.obligations

-- | All the campaign adds to the player's deck: its log's cards, and the Market cards
-- they bought.
campaignCardsOf :: CardCode -> Campaign -> [CardCode]
campaignCardsOf ident c =
  foldMap campaignCards (campaignPlayer ident c)
    <> foldMap (.marketCards) (galaxyPlayer ident c)
    <> foldMap sinisterCampaignCards (sinisterPlayer ident c)
    <> apocalypseRewardsFor ident c

-- | Tech and Aspect Advantage are extra deck cards. Planning Ahead only names a
-- card already in the deck; retrieving it does not add another copy.
sinisterCampaignCards :: SinisterPlayer -> [CardCode]
sinisterCampaignCards p =
  maybeToList p.shieldTech
    <> foldMap (\(code, copies) -> replicate (fromIntegral copies) code) p.aspectAdvantage

sinisterPlayer :: CardCode -> Campaign -> Maybe SinisterPlayer
sinisterPlayer ident c = find ((== toHeroCardCode ident) . (.identity)) c.campaignLog.sinister.sinisterPlayers

-- | Nodes with connected effects, visually checked against the printed track p.22.
reputationThresholds :: [Int]
reputationThresholds = [1, 5, 9, 13, 17, 21, 25]

-- | Newly marked connected nodes, in track order. A reward at an old node is not
-- earned again simply because the group gains more reputation later.
crossedReputationThresholds :: Int -> Int -> [Int]
crossedReputationThresholds previous current = filter (\n -> previous < n && n <= current) reputationThresholds

-- | The eight physical Tech cards, by their unenhanced faces. Unchosen cards return
-- to the collection before the next player is dealt their three (MC27 p.22).
shieldTechUpgrades :: [CardCode]
shieldTechUpgrades = ["27182a", "27183a", "27184a", "27185a", "27186a", "27187a", "27188a", "27189a"]

shieldTechLeft :: Campaign -> [CardCode]
shieldTechLeft c = filter ((`notElem` chosen) . toBaseCardCode) shieldTechUpgrades
 where
  chosen = map toBaseCardCode $ mapMaybe (.shieldTech) c.campaignLog.sinister.sinisterPlayers

osbornTechAttachments :: [CardCode]
osbornTechAttachments = ["27147", "27148", "27149", "27150", "27151", "27152"]

osbornTechLeft :: Campaign -> [CardCode]
osbornTechLeft c = filter (`notElem` c.campaignLog.sinister.osbornTech) osbornTechAttachments

communityServiceSideSchemes :: [CardCode]
communityServiceSideSchemes = ["27176", "27177", "27178", "27179", "27180"]

communityServiceLeft :: Campaign -> [CardCode]
communityServiceLeft c = filter (`notElem` c.campaignLog.sinister.communityService) communityServiceSideSchemes

-- | The villains' matching Sinister Assault minions, in activation order. Record
-- the villain cards at the Six's victory; put the matching minions into Goblin's deck.
sinisterAssaultVillains :: [(CardCode, CardCode)]
sinisterAssaultVillains = zip ["27094", "27095", "27096", "27097", "27098", "27099"] ["27158", "27159", "27160", "27161", "27162", "27163"]

lastOnesStandingMinions :: Campaign -> [CardCode]
lastOnesStandingMinions c = mapMaybe (`List.lookup` sinisterAssaultVillains) c.campaignLog.sinister.lastOnesStanding

-- | The campaign's record written in its log.
recordInLog :: CampaignRecord -> Campaign -> Campaign
recordInLog r c = let mutant f = c {campaignLog = c.campaignLog {mutantGenesis = f c.campaignLog.mutantGenesis}} in case r of
  RecordShieldLog shieldLog -> c {campaignLog = c.campaignLog {shield = shieldLog}}
  RecordCampaignLost -> c {lost = True}
  RecordApocalypseMission n code -> aoa \l -> l {missions = filter ((/= n) . fst) l.missions <> [(n, code)]}
  RecordApocalypseMissionSuccess code -> aoa \l -> l {successfulMissions = nub (l.successfulMissions <> [code])}
  RecordApocalypseOverseer code -> aoa \l -> l {defeatedOverseers = nub (l.defeatedOverseers <> [code])}
  RecordApocalypseReward ident code -> aoa \l ->
    let hero = toHeroCardCode ident
        previous = fromMaybe [] (List.lookup hero l.apocalypseRewards)
    in l {apocalypseRewards = filter ((/= hero) . fst) l.apocalypseRewards <> [(hero, nub (previous <> [code]))]}
  RemoveApocalypseReward code -> aoa \l -> l {apocalypseRewards = [(hero, filter (/= code) cards) | (hero, cards) <- l.apocalypseRewards]}
  RecordNextEvolutionScheme n code -> ne \l -> l {chosenSchemes = filter ((/= n) . fst) l.chosenSchemes <> [(n, code)]}
  RecordNextEvolutionEnvironment code -> ne \l -> l {earnedEnvironments = nub (l.earnedEnvironments <> [code])}
  RecordNextEvolutionMarauders codes -> ne \l -> l {maraudersDefeated = codes}
  RecordNextEvolutionMorlocks n -> ne \l -> l {morlocksSaved = n}
  RecordNextEvolutionHopeDamage n damage -> ne \l -> l {hopeDamage = filter ((/= n) . fst) l.hopeDamage <> [(n, damage)]}
  RecordMutantRole ident r -> mutant \l -> l {mutantPlayers = [if p.identity == toHeroCardCode ident then p {role = Just r} else p | p <- l.mutantPlayers]}
  RecordMutantSchemeDefeated code -> mutant \l -> l {defeatedSchemes = nub (l.defeatedSchemes <> [code])}
  RecordFuturePast active removed -> mutant \l -> l {futurePastActive = active, futurePastRemoved = nub (l.futurePastRemoved <> removed)}
  RecordFuturePastDeck codes -> mutant \l -> l {futurePastDeck = codes}
  RecordMutantRoleUpgrade code -> mutant \l -> l {roleUpgradesInPlay = nub (l.roleUpgradesInPlay <> [code])}
  RemoveMutantRoleUpgrade code -> mutant \l -> l {roleUpgradesRemoved = nub (l.roleUpgradesRemoved <> [code]), roleUpgradesInPlay = filter (/= code) l.roleUpgradesInPlay}
  RecordMutantJubilee present -> mutant \l -> l {jubileeInPlay = present}
  RecordMutantCaptives codes -> mutant \l -> l {captiveAllies = nub (l.captiveAllies <> codes)}

  RecordHitPoints ident n -> player ident \p -> p {hitPoints = Just n}
  RecordExperimentalWeapons codes -> logged \l -> l {experimentalWeapons = nub (l.experimentalWeapons <> codes)}
  RecordDelayCounters n -> logged \l -> l {delayCounters = Just n}
  RecordEngagedWithMinions ident -> logged \l -> l {engagedWithMinions = nub (l.engagedWithMinions <> [toHeroCardCode ident])}
  RecordAlliesRemoved codes ->
    logged \l ->
      l
        { alliesRemoved = nub (l.alliesRemoved <> codes)
        , players = [p {rescuedAllies = filter (`notElem` codes) p.rescuedAllies} | p <- l.players]
        }
  RecordRescuedAllies ident codes -> player ident \p -> p {rescuedAllies = nub (p.rescuedAllies <> codes)}
  RecordTechUpgrade ident code -> player ident \p -> p {techUpgrade = Just code}
  RecordConditionUpgrade ident code -> player ident \p -> p {conditionUpgrade = Just code}
  RecordObligation ident code -> player ident \p -> p {obligations = p.obligations <> [code]}
  RemoveFromCampaignLog ident code ->
    player ident \p ->
      p
        { techUpgrade = mfilter (/= code) p.techUpgrade
        , conditionUpgrade = mfilter (/= code) p.conditionUpgrade
        , rescuedAllies = filter (/= code) p.rescuedAllies
        , obligations = filter (/= code) p.obligations
        }
  RecordChoice ident choice -> player ident \p -> p {pending = p.pending <> [choice]}
  ResolvedChoice ident choice -> player ident \p -> p {pending = filter (/= choice) p.pending}
  RecordScenarioWon -> c {completed = c.completed + 1}
  RecordUnits ident n -> galaxyP ident \p -> p {units = p.units + n}
  SpendUnits ident n -> galaxyP ident \p -> p {units = p.units - min n p.units}
  BuyMarketCard ident code
    | canBuy ident code c ->
        galaxyP ident \p -> p {units = p.units - fromMaybe 0 (List.lookup code marketUnitCosts), marketCards = p.marketCards <> [code]}
    | otherwise -> c
  RecordCollection ident codes -> galaxyP ident \p -> p {collection = p.collection <> codes}
  RecordGalacticArtifacts codes -> galaxyL \g -> g {galacticArtifacts = nub (g.galacticArtifacts <> codes)}
  RecordPowerStoneControl ident -> galaxyL \g -> g {powerStoneControl = Just (toHeroCardCode ident)}
  RecordHeadhunterDefeated n -> galaxyL \g -> g {headhunterDefeated = nub (g.headhunterDefeated <> [n])}
  RecordEvasionCounters n -> galaxyL \g -> g {evasionCounters = Just n}
  RecordTitanMark m -> logged \l -> l {titan = TitanLog (nub (l.titan.marks <> [m]))}
  RecordReputation n -> sinisterL \s -> s {reputation = s.reputation + n}
  RecordCommunityService codes -> sinisterL \s ->
    s {communityService = nub (s.communityService <> filter (`elem` communityServiceSideSchemes) codes)}
  RecordOsbornTech code
    | code `elem` osbornTechAttachments -> sinisterL \s -> s {osbornTech = nub (s.osbornTech <> [code])}
    | otherwise -> c
  RecordShieldTech ident code
    | code `elem` shieldTechLeft c -> sinisterP ident \p -> p {shieldTech = Just code}
    | otherwise -> c
  RecordAspectAdvantage ident code copies -> sinisterP ident \p -> p {aspectAdvantage = Just (code, copies)}
  RecordPlanningAhead ident code -> sinisterP ident \p -> p {planningAhead = Just code}
  RecordWakingNightmare n -> sinisterL \s -> s {wakingNightmare = Just n}
  RecordLastOnesStanding codes -> sinisterL \s -> s {lastOnesStanding = nub $ filter (`elem` map fst sinisterAssaultVillains) codes}
  RecordFinalReputation -> sinisterL \s -> s {finalReputation = Just s.reputation}
 where
  logged f = c {campaignLog = f c.campaignLog}
  aoa f = logged \l -> l {apocalypse = f l.apocalypse}
  ne f = logged \l -> l {nextEvolution = f l.nextEvolution}
  sinisterL f = logged \l -> l {sinister = f l.sinister}
  sinisterP ident f = sinisterL \s -> s {sinisterPlayers = [if p.identity == toHeroCardCode ident then f p else p | p <- s.sinisterPlayers]}
  galaxyL f = logged \l -> l {galaxy = f l.galaxy}
  galaxyP ident f = galaxyL \g -> g {galaxyPlayers = [if p.identity == toHeroCardCode ident then f p else p | p <- g.galaxyPlayers]}
  player ident f = logged \l -> l {players = [if p.identity == toHeroCardCode ident then f p else p | p <- l.players]}
