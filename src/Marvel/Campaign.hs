-- | A campaign: scenarios played one after another, what each game leaves behind kept in
-- a campaign log ("docs/rules/campaign-rise-of-red-skull.md"). The campaign lives outside
-- the game: the server keeps it between games, and each game of it is dealt with a copy
-- to read as it is set up and to write in as it is won.
module Marvel.Campaign where

import Marvel.Prelude

import Data.List (find, nub)
import Control.Monad (mfilter)
import Data.Maybe (maybeToList)
import Marvel.Card.Code

data CampaignMode = StandardCampaign | ExpertCampaign
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON)

-- | A choice a victory leaves a player, asked as the next game is set up.
data CampaignChoice
  = -- | Crossbones: one of the Tech upgrades of the Hydra Campaign set
    ChooseTechUpgrade
  | -- | Absorbing Man: may choose one of the "Basic" Condition upgrades
    ChooseBasicUpgrade
  | -- | Zola: may replace the "Basic" Condition upgrade with its "Improved" side
    MayImproveUpgrade
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
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON)

data Campaign = Campaign
  { mode :: CampaignMode
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
data CampaignStepKind = CampaignChoices | CampaignSetup | CampaignVictory
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON)

-- | What a scenario writes in the log, the player named by their identity's code.
data CampaignRecord
  = RecordHitPoints CardCode Natural
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
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON)

-- | The Rise of Red Skull, by its villains: Crossbones, Absorbing Man, Taskmaster, Zola,
-- Red Skull.
riseOfRedSkull :: [CardCode]
riseOfRedSkull = ["04058", "04076", "04093", "04109", "04125"]

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

-- | A new campaign for the identities, numbered in the order given.
newCampaign :: CampaignMode -> [CardCode] -> Campaign
newCampaign mode identities =
  Campaign
    { mode
    , scenarios = riseOfRedSkull
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

-- | The campaign's record written in its log.
recordInLog :: CampaignRecord -> Campaign -> Campaign
recordInLog r c = case r of
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
 where
  logged f = c {campaignLog = f c.campaignLog}
  player ident f = logged \l -> l {players = [if p.identity == toHeroCardCode ident then f p else p | p <- l.players]}
