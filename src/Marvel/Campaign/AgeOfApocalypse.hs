-- | MC45 campaign instructions and the mission log, rulebook pp.4–6,8,12,14,16,20,24.
module Marvel.Campaign.AgeOfApocalypse (runApocalypseStep) where

import Marvel.Prelude
import Data.HashMap.Strict qualified as HashMap
import Data.List qualified as List
import Data.Text qualified as T
import Marvel.AgeOfApocalypse.MissionTypes
import Marvel.Campaign
import Marvel.Card
import Marvel.EncounterCard (allEncounterCards, genEncounterCard, gatherEncounterSet)
import Marvel.EncounterSet qualified as Set
import Marvel.Game.Source
import Marvel.Id
import Marvel.Matchers
import Marvel.Message.Lifted
import Marvel.PlayerCard (lookupPlayerCard, allPlayerCards)
import Marvel.Query
import Marvel.Ref
import System.Random.Shuffle (shuffleM)
import Text.Read (readMaybe)

runApocalypseStep :: ReverseQueue m => Campaign -> CampaignStepKind -> m ()
runApocalypseStep c = \case
  CampaignChoices -> selectJust FirstPlayer >>= (\you -> readThenContinue you $ story c "setup")
  CampaignSetup -> do
    shuffleIntoEncounterDeck =<< gatherEncounterSet Set.AgeOfApocalypse
    let previous = List.lookup (scenarioNumber c) c.campaignLog.apocalypse.missions
        available = filter (`notElem` map snd c.campaignLog.apocalypse.missions) apocalypseMissionCards
    missions <- maybe (shuffleM available) (pure . pure) previous
    overseers <- shuffleM $ filter (`notElem` c.campaignLog.apocalypse.defeatedOverseers) apocalypseOverseers
    let mission = if scenarioNumber c == 5 then Just "45170a" else headMay missions
    for_ ((,) <$> mission <*> headMay overseers) \(code, overseer) -> do
      recordInCampaignLog (RecordApocalypseMission (scenarioNumber c) code)
      cardSpecific GameRef "aoaMissionSetup" (code, overseer)
      later "missionSetup"
  CampaignVictory -> do
    first <- selectJust FirstPlayer
    readThenContinue first (story c "victory")
    later "victory"
  CampaignSetupThen step -> case T.stripPrefix "aoa:" step of
    Just "missionSetup" -> do
      for_ (List.lookup (scenarioNumber c) c.campaignLog.apocalypse.missions) \mission -> do
        when (mission == "45167a") $ forPlayers \you -> addRefugees you
        when (mission == "45168a" || failedEarlier c "45168a") $ shuffleEncounter "45177"
      -- This is before SetupPlayers draws to the hand limit: the searched ally
      -- replaces a starting card instead of adding an extra one.
      forPlayers \you -> do
        code <- getIdentityCode you
        for_ (filter (/= "45178") $ apocalypseRewardsFor code c) $ \reward -> cardSpecific GameRef "aoaOptionalCampaignCard" (you, reward)
        cardSpecific GameRef "aoaStartingAlly" (you, isExpertCampaign c)
      later "expert"
    Just "expert" -> expertSetup c
    Just "victory" -> victory c
    Just "rewardUpgrade" -> chooseReward c UpgradeType
    Just "rewardSupport" -> chooseReward c SupportType
    Just "rewardAlly" -> chooseCampaignAllies c 0
    Just value | Just n <- T.stripPrefix "rewardAlly:" value >>= readMaybe . T.unpack -> chooseCampaignAllies c n
    Just "recordWin" -> recordInCampaignLog RecordScenarioWon
    _ -> pure ()

story :: Campaign -> Text -> [Text]
story c section = ["$story.ageOfApocalypse." <> fromMaybe "enSabahNur" (atMay ["unus", "fourHorsemen", "apocalypse", "darkBeast"] (scenarioNumber c - 1)) <> "." <> section]

later :: ReverseQueue m => Text -> m ()
later = campaignStep . CampaignSetupThen . ("aoa:" <>)

forPlayers :: ReverseQueue m => (IdentityId -> m ()) -> m ()
forPlayers f = getPlayers >>= traverse_ f

failedEarlier :: Campaign -> CardCode -> Bool
failedEarlier c code = code `elem` map snd c.campaignLog.apocalypse.missions && code `notElem` c.campaignLog.apocalypse.successfulMissions

encounterDef :: CardCode -> CardDef
encounterDef code = fromMaybe (error $ "Unregistered Age of Apocalypse card: " <> show code) (HashMap.lookup code allEncounterCards)

shuffleEncounter :: ReverseQueue m => CardCode -> m ()
shuffleEncounter code = shuffleIntoEncounterDeck . pure =<< genEncounterCard (encounterDef code)

playerCard :: MonadRandom m => IdentityId -> CardCode -> m PlayerCard
playerCard you code = do
  cid <- getRandom
  pure $ MkPlayerCard cid (lookupPlayerCard code) (Just you) (Just you)

addRefugees :: ReverseQueue m => IdentityId -> m ()
addRefugees you = shuffleIntoDeck you . pure =<< playerCard you "45178"

expertSetup :: ReverseQueue m => Campaign -> m ()
expertSetup c = when (isExpertCampaign c && scenarioNumber c > 1) $ forPlayers \you -> do
  code <- getIdentityCode you
  (_, full, _) <- getRemainingHitPoints you
  for_ (campaignPlayer code c >>= (.hitPoints)) \hp -> do
    mission <- getMission
    for_ mission \m -> do
      let restore = placeThreat m.missionScheme 3 >> setHitPoints you full
      if hp == 0 then restore else do
        setHitPoints you hp
        when (hp < full) $ chooseOnePrompted you "$label.expertCampaignHeal"
          [labeled "$label.aoaThreatToHeal" restore, labeled "$label.keepHitPoints" (pure ())]

victory :: ReverseQueue m => Campaign -> m ()
victory c = do
  mission <- getMission
  for_ mission \m -> do
    when m.missionSucceeded $ recordInCampaignLog (RecordApocalypseMissionSuccess m.missionCard)
    display <- getVictoryDisplay
    when (any ((== m.missionOverseer) . toCardCode) display) $ recordInCampaignLog (RecordApocalypseOverseer m.missionOverseer)
    if scenarioNumber c == 5 then do
      unless m.missionSucceeded $ recordInCampaignLog RecordCampaignLost
    else case m.missionCard of
      "45166a" | m.missionSucceeded -> forPlayers \you -> do
        code <- getIdentityCode you
        recordInCampaignLog (RecordApocalypseReward code "45176")
      "45167a" | m.missionSucceeded -> do
        recordInCampaignLog (RemoveApocalypseReward "45178")
        later "rewardUpgrade"
      "45167a" -> forPlayers \you -> getIdentityCode you >>= \code -> recordInCampaignLog (RecordApocalypseReward code "45178")
      "45168a" | m.missionSucceeded -> later "rewardSupport"
      "45169a" | m.missionSucceeded -> later "rewardAlly"
      _ -> pure ()
  when (isExpertCampaign c && scenarioNumber c < 5) do
    survivors <- getPlayers
    codes <- traverse getIdentityCode survivors
    for_ c.campaignLog.players \p -> unless (p.identity `elem` codes) $ recordInCampaignLog (RecordHitPoints p.identity 0)
    for_ survivors \you -> do
      (remaining, _, printed) <- getRemainingHitPoints you
      code <- getIdentityCode you
      recordInCampaignLog (RecordHitPoints code $ min remaining printed)
  later "recordWin"

chooseReward :: ReverseQueue m => Campaign -> CardType -> m ()
chooseReward _ typ = forPlayers \you -> do
  code <- getIdentityCode you
  let defs = List.nubBy (\a b -> sameCard a b)
        [def | def <- HashMap.elems allPlayerCards, cdCardType def == typ, isJust (cdAspect def), isNothing (cdEncounterSet def)]
  unless (null defs) $ chooseOnePrompted you "$label.aoaChooseCampaignReward"
    [cardLabeled (toCardCode def) $ recordInCampaignLog (RecordApocalypseReward code $ toCardCode def) | def <- defs]

chooseCampaignAllies :: ReverseQueue m => Campaign -> Int -> m ()
chooseCampaignAllies c i = do
  let claimed = concatMap snd c.campaignLog.apocalypse.apocalypseRewards
      available = filter (`notElem` claimed) ["45172", "45173", "45174", "45175"]
  players <- getPlayers
  for_ (atMay players i) \you -> do
    hero <- getIdentityCode you
    unless (null available) $ chooseOnePrompted you "$label.aoaChooseCampaignAlly"
      [cardLabeled code do
        recordInCampaignLog (RecordApocalypseReward hero code)
        later ("rewardAlly:" <> tshow (i + 1))
      | code <- available]
