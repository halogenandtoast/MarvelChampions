-- | Sinister Motives campaign instructions (MC27 pp.4–6, 9–17, 22–23).
module Marvel.Campaign.SinisterMotives (runSinisterStep, reputationScore) where

import Marvel.Prelude

import Control.Monad (replicateM, replicateM_)
import Data.Function (on)
import Data.HashMap.Strict qualified as HashMap
import Data.List (nubBy, sortOn)
import Data.List qualified as List
import Data.Text qualified as T
import Marvel.Aspect (Aspect (Neutral))
import Marvel.Campaign
import Marvel.Card
import Marvel.Count
import Marvel.Deck
import Marvel.Difficulty
import Marvel.EncounterCard (allEncounterCards, genEncounterCard)
import Marvel.Game.Source
import Marvel.Id
import Marvel.Identity.Types (Field (..))
import Marvel.Matchers
import Marvel.Message.Lifted
import Marvel.Name (Name (..))
import Marvel.PlayerCard (allPlayerCards, lookupPlayerCard)
import Marvel.Projection
import Marvel.Query
import Marvel.Ref
import Marvel.Trait (Trait (Illusion))
import Marvel.Villain.Cards qualified as Villains
import System.Random.Shuffle (shuffleM)
import Text.Read (readMaybe)

runSinisterStep :: (ReverseQueue m) => Campaign -> CampaignStepKind -> m ()
runSinisterStep c = \case
  CampaignChoices -> do
    players <- getPlayers
    for_ (take 1 players) (`readThenContinue` briefing c)
    for_ (zip [0 :: Int ..] players) \(i, _) -> later ("pending:" <> tshow i)
  CampaignSetup -> setup c
  CampaignVictory -> victory c
  CampaignSetupThen step -> case T.stripPrefix "sinister:" step of
    Just "reputationLeft" -> reputationLeft c
    Just "reputationRight" -> reputationRight c
    Just "expert" -> expertSetup c
    Just "scenarioSideScheme" -> scenarioSideScheme
    Just "rightEncounters" -> when (c.campaignLog.sinister.reputation >= 25) $ getPlayers >>= traverse_ dealEncounterCard
    Just "reward1" -> rewardEach c ChooseSinisterTech
    Just "reward9" -> rewardEach c ChooseSinisterAspectAdvantage
    Just "reward17" -> rewardEach c ChooseSinisterPlanningAhead
    Just "osborn" -> do
      available <- shuffleM (osbornTechLeft c)
      for_ (take 1 available) (recordInCampaignLog . RecordOsbornTech)
    Just "finishVictory" -> finishVictory c
    Just value | Just i <- suffixNumber "pending:" value -> do
      players <- getPlayers
      for_ (atMay players i) \ident -> do
        code <- getIdentityCode ident
        for_ (campaignPlayer code c) \p ->
          for_ (filter sinisterChoice p.pending) (offerReward True c ident code)
    Just value | Just i <- suffixNumber "rewardTech:" value -> rewardAt c ChooseSinisterTech i
    Just value | Just i <- suffixNumber "rewardAspect:" value -> rewardAt c ChooseSinisterAspectAdvantage i
    Just value | Just i <- suffixNumber "rewardPlanning:" value -> rewardAt c ChooseSinisterPlanningAhead i
    Just value | Just i <- suffixNumber "minion:" value -> do
      players <- getPlayers
      for_ (atMay players i) chooseMinion
    Just value | Just code <- T.stripPrefix "sideThreat:" value -> do
      schemes <- selectList (SideSchemeIs $ encounterDef $ CardCode code)
      for_ schemes \sid -> placeThreat sid (fromIntegral $ length c.campaignLog.players)
    _ -> pure ()

suffixNumber :: Text -> Text -> Maybe Int
suffixNumber prefix value = T.stripPrefix prefix value >>= readMaybe . T.unpack

later :: (ReverseQueue m) => Text -> m ()
later = campaignStep . CampaignSetupThen . ("sinister:" <>)

sinisterChoice :: CampaignChoice -> Bool
sinisterChoice = \case
  ChooseSinisterTech -> True
  ChooseSinisterAspectAdvantage -> True
  ChooseSinisterPlanningAhead -> True
  _ -> False

storyKeys :: Campaign -> Text -> [Text]
storyKeys c ending = ["$story.sm." <> scenario <> "." <> ending]
 where
  scenario = fromMaybe "venomGoblin" $ atMay ["sandman", "venom", "mysterio", "sinisterSix"] (scenarioNumber c - 1)

briefing :: Campaign -> [Text]
briefing c = storyKeys c "intro"
  <> ["$story.sm.campaignMode" | scenarioNumber c == 1]
  <> ["$story.sm.expertCampaign" | scenarioNumber c == 1 && isExpertCampaign c]
  <> storyKeys c "rules" <> storyKeys c "setup"
  <> [key | isExpertCampaign c, key <- storyKeys c "expertSetup"]

encounterDef :: CardCode -> CardDef
encounterDef code = fromMaybe (error $ "Sinister Motives encounter card is not registered: " <> show code) $ HashMap.lookup code allEncounterCards

newCard :: (MonadRandom m) => IdentityId -> CardCode -> m PlayerCard
newCard ident code = do
  cid <- getRandom
  pure $ MkPlayerCard cid (lookupPlayerCard code) (Just ident) (Just ident)

shuffleEncounter :: (ReverseQueue m) => CardCode -> m ()
shuffleEncounter code = shuffleIntoEncounterDeck . pure =<< genEncounterCard (encounterDef code)

-- * Setup

setup :: (ReverseQueue m) => Campaign -> m ()
setup c = do
  first <- selectJust FirstPlayer
  let n = scenarioNumber c
  when (n `elem` [3, 4]) $ putIntoPlay first =<< newCard first "27190"
  difficulty <- getDifficulty
  putEncounterCardIntoPlay first =<< genEncounterCard (encounterDef $ if difficulty == Expert then "27174b" else "27174a")
  shuffleEncounter "27175"
  when (n `elem` [3, 4]) $ shuffleEncounter "27181"
  when (n <= 4) do
    available <- shuffleM (communityServiceLeft c)
    traverse_ shuffleEncounter (take 1 available)
  when (n == 4) do
    schemes <- selectList (SideSchemeIs $ encounterDef "27102a")
    for_ schemes \sid -> placeThreat sid (fromMaybe 0 c.campaignLog.sinister.wakingNightmare)
  when (n == 5) $ traverse_ shuffleEncounter (lastOnesStandingMinions c)
  when (n > 1) do
    later "reputationLeft"
    later "reputationRight"
  later "expert"

reputationLeft :: (ReverseQueue m) => Campaign -> m ()
reputationLeft c = do
  players <- getPlayers
  let score = c.campaignLog.sinister.reputation
  when (score >= 13) $ for_ players \ident -> do
    code <- getIdentityCode ident
    for_ ((.shieldTech) =<< sinisterPlayer code c) \tech -> do
      upgrades <- selectList (UpgradeWithCard $ CardIs $ lookupPlayerCard tech)
      for_ upgrades \uid -> cardSpecific uid "enhance" ()
  when (score >= 17) $ for_ players \ident -> do
    code <- getIdentityCode ident
    for_ ((.planningAhead) =<< sinisterPlayer code c) \planned ->
      search ident (SearchIdentityDeckAndDiscardPile ident)
        (CardWithTitle $ title $ cdName $ lookupPlayerCard planned) SearchDrawOne ShuffleBackIn
  when (score >= 21) $ for_ players \ident -> optionalIntoPlay ident "01092"
  when (score >= 25) $ for_ players \ident -> optionalIntoPlay ident "27191"

optionalIntoPlay :: (ReverseQueue m) => IdentityId -> CardCode -> m ()
optionalIntoPlay ident code =
  chooseOnePrompted ident "$label.sinisterOptionalSetup"
    [cardLabeled code (putIntoPlay ident =<< newCard ident code), labeled "$label.pass" (pure ())]

reputationRight :: (ReverseQueue m) => Campaign -> m ()
reputationRight c = do
  players <- getPlayers
  let score = c.campaignLog.sinister.reputation
      count = fromIntegral $ length c.campaignLog.players
  when (score >= 1) $ traverse_ shuffleEncounter c.campaignLog.sinister.osbornTech
  when (score >= 5) $ selectOne MainScheme >>= traverse_ (\scheme -> placeThreat scheme count)
  when (score >= 9) $ for_ (zip [0 :: Int ..] players) \(i, _) -> later ("minion:" <> tshow i)
  when (score >= 17) $ later "scenarioSideScheme"
  later "rightEncounters"

-- | Search after earlier minion choices and their enter-play windows have resolved.
scenarioSideScheme :: (ReverseQueue m) => m ()
scenarioSideScheme = do
  first <- selectJust FirstPlayer
  deck <- getEncounterDeck
  discard <- getEncounterDiscard
  scenario <- getScenarioCode
  let villainCode = if scenario == "27100" then "27094" else scenario
      set = cdEncounterSet $ fromMaybe (error "campaign scenario villain is not registered") $ HashMap.lookup villainCode Villains.allVillainsMap
      found = nubBy ((==) `on` toCardCode)
        [card | card <- deck <> discard, cdCardType (getCardDef card) == SideSchemeType, cdEncounterSet (getCardDef card) == set]
  if null found
    then shuffleEncounterDeck
    else chooseOnePrompted first "$label.sinisterScenarioSideScheme"
      [ targeting card do
          revealEncounterCard first card
          later ("sideThreat:" <> unCardCode (toCardCode card))
          shuffleEncounterDeck
      | card <- found
      ]

chooseMinion :: (ReverseQueue m) => IdentityId -> m ()
chooseMinion ident = do
  deck <- getEncounterDeck
  discard <- getEncounterDiscard
  let minions = nubBy ((==) `on` toCardCode) $ filter ((== MinionType) . cdCardType . getCardDef) (deck <> discard)
  if null minions
    then dealEncounterCard ident >> shuffleEncounterDeck
    else chooseOnePrompted ident "$label.sinisterChooseMinion"
      [targeting card (putEncounterCardIntoPlay ident card >> shuffleEncounterDeck) | card <- minions]

expertSetup :: (ReverseQueue m) => Campaign -> m ()
expertSetup c = when (isExpertCampaign c) do
  players <- getPlayers
  let n = scenarioNumber c
      encounterCost = if n == 2 then 1 else if n == 5 then 3 else 2
  when (n > 1) $ for_ players \ident -> do
    code <- getIdentityCode ident
    (_, _, printed) <- getRemainingHitPoints ident
    for_ ((.hitPoints) =<< campaignPlayer code c) \hp -> do
      let heal = replicateM_ encounterCost (dealEncounterCard ident) >> setHitPoints ident printed
      if hp == 0
        then heal
        else do
          setHitPoints ident hp
          when (hp < printed) $
            chooseOnePrompted ident "$label.sinisterExpertHeal"
              [labeled "$label.sinisterTakeEncountersToHeal" heal, labeled "$label.keepHitPoints" (pure ())]
  case n of
    1 -> do
      streets <- selectList (EnvironmentWithTitle "City Streets")
      for_ streets \eid -> do
        cardSpecific eid "placeSand" (2 :: Natural)
        cardSpecific eid "surgingSands" ()
    2 -> for_ players \ident -> giveIdentityBoostCards ident 1
    3 -> for_ players \ident -> shuffleTopEncounterCardsIntoPlayerDeck ident 2
    5 -> selectList MainScheme >>= traverse_ (\scheme -> placeThreat scheme (fromIntegral $ length c.campaignLog.players))
    _ -> pure ()

-- * Reputation rewards

rewardEach :: (ReverseQueue m) => Campaign -> CampaignChoice -> m ()
rewardEach c choice = for_ (zip [0 :: Int ..] c.campaignLog.players) \(i, _) -> later (prefix <> tshow i)
 where
  prefix = case choice of
    ChooseSinisterTech -> "rewardTech:"
    ChooseSinisterAspectAdvantage -> "rewardAspect:"
    _ -> "rewardPlanning:"

rewardAt :: (ReverseQueue m) => Campaign -> CampaignChoice -> Int -> m ()
rewardAt c choice i = for_ (atMay c.campaignLog.players i) \p -> do
  players <- getPlayers
  codes <- traverse getIdentityCode players
  case List.lookup p.identity (zip codes players) of
    Just ident -> offerReward False c ident p.identity choice
    Nothing -> unless (isExpertCampaign c) $ recordInCampaignLog (RecordChoice p.identity choice)

-- | At victory, only write the reward. A choice left for the next setup must also
-- put the newly selected cards in the already constructed deck before setup cards.
offerReward :: (ReverseQueue m) => Bool -> Campaign -> IdentityId -> CardCode -> CampaignChoice -> m ()
offerReward inNextGame c ident code choice = case choice of
  ChooseSinisterTech -> do
    available <- take 3 <$> shuffleM (shieldTechLeft c)
    chooseOnePrompted ident "$label.sinisterChooseTech" $
      [ cardLabeled tech do
          recordInCampaignLog (RecordShieldTech code tech)
          when inNextGame $ shuffleIntoDeck ident . pure =<< newCard ident tech
          done
      | tech <- available
      ] <> [labeled "$label.pass" done]
  ChooseSinisterAspectAdvantage -> do
    let eligible def = maybe False (/= Neutral) (cdAspect def)
        cards = sortOn cdCardCode $ filter eligible $ HashMap.elems allPlayerCards
        choices = nubBy ((==) `on` cdName) cards
    unless (null choices) $ chooseOnePrompted ident "$label.sinisterAspectAdvantage"
      [ cardLabeled (cdCardCode def) do
          let copies = fromMaybe (if cdUnique def then 1 else 3) (cdDeckLimit def)
          recordInCampaignLog (RecordAspectAdvantage code (cdCardCode def) copies)
          when inNextGame $ do
            added <- replicateM (fromIntegral copies) (newCard ident $ cdCardCode def)
            shuffleIntoDeck ident added
          done
      | def <- choices
      ]
  ChooseSinisterPlanningAhead -> do
    -- "Deck" here is the deck's card composition, including cards currently in
    -- hand, discard, play or held beneath other cards, and irrespective of controller.
    owned <- getCardsOwnedBy ident
    let choices = nubBy ((==) `on` cdName) owned
    unless (null choices) $ chooseOnePrompted ident "$label.sinisterPlanningAhead"
      [cardLabeled (cdCardCode def) (recordInCampaignLog (RecordPlanningAhead code $ cdCardCode def) >> done) | def <- choices]
  _ -> pure ()
 where
  done = recordInCampaignLog (ResolvedChoice code choice)

-- * Victory

reputationScore :: (HasGame m) => Campaign -> m Int
reputationScore c = do
  points <- getVictoryPoints
  noMinions <- selectNone AnyMinion
  noSides <- selectNone AnySideScheme
  noThreat <- (== 0) <$> selectCount SchemeThreat MainScheme
  tokens <- getAccelerationTokenCount
  survivors <- getPlayers
  let players = length c.campaignLog.players
      conditions = [noMinions, noSides, noThreat, tokens < fromIntegral players, length survivors == players]
  pure $ points + length (filter id conditions)

victory :: (ReverseQueue m) => Campaign -> m ()
victory c = do
  players <- getPlayers
  for_ (take 1 players) (`readThenContinue` (storyKeys c "victory" <> ["$story.sm.conclusion" | scenarioNumber c == 5]))
  if scenarioNumber c == 5
    then for_ (take 1 players) \first ->
      chooseOnePrompted first "$label.sinisterFinalScore"
        [ labeled "$label.sinisterCalculateFinalScore" (scoreReputation c >> later "finishVictory")
        , labeled "$label.pass" (recordInCampaignLog RecordScenarioWon)
        ]
    else scoreReputation c >> later "finishVictory"

scoreReputation :: (ReverseQueue m) => Campaign -> m ()
scoreReputation c = do
  -- Project owner, 2026-10-30: floor the earned amount at zero; keep prior nodes.
  earned <- max 0 <$> reputationScore c
  recordInCampaignLog (RecordReputation earned)
  let previous = c.campaignLog.sinister.reputation
  for_ (crossedReputationThresholds previous (previous + earned)) \node -> case node of
    1 -> later "reward1" >> later "osborn"
    9 -> later "reward9"
    13 -> later "osborn"
    17 -> later "reward17"
    21 -> later "osborn"
    _ -> pure ()

finishVictory :: (ReverseQueue m) => Campaign -> m ()
finishVictory c = do
  let n = scenarioNumber c
  if n == 5
    then recordInCampaignLog RecordFinalReputation
    else do
      display <- map toCardCode <$> getVictoryDisplay
      recordInCampaignLog (RecordCommunityService $ filter (`elem` communityServiceSideSchemes) display)
      when (n == 3) do
        players <- getAllPlayers
        decks <- concatMapM (projectMap PlayerIdentityDeck unDeck) players
        let illusions = filter (member Illusion . cdTraits . getCardDef) decks
        recordInCampaignLog (RecordWakingNightmare $ fromIntegral $ length illusions)
      when (n == 4) do
        villains <- selectList AnyVillain
        defs <- catMaybes <$> traverse (\vid -> getsGame (`abilityCardDef` VillainRef vid)) villains
        recordInCampaignLog (RecordLastOnesStanding $ map cdCardCode defs)
      when (isExpertCampaign c) do
        survivors <- getPlayers
        codes <- traverse getIdentityCode survivors
        for_ (zip survivors codes) \(ident, code) -> do
          (remaining, _, printed) <- getRemainingHitPoints ident
          recordInCampaignLog (RecordHitPoints code $ min remaining printed)
        for_ c.campaignLog.players \p -> unless (p.identity `elem` codes) $ recordInCampaignLog (RecordHitPoints p.identity 0)
  recordInCampaignLog RecordScenarioWon
