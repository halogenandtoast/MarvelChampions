module Marvel.Campaign.FearNoEvil (runFearNoEvilStep,campaignEnvironmentFor) where

import Marvel.Prelude
import Data.List qualified as List
import Data.HashMap.Strict qualified as HashMap
import Marvel.AlterEgo.Runner qualified as AE
import Marvel.Campaign
import Marvel.Card
import Marvel.Difficulty
import Marvel.Trait (Trait(Prisoner,DailyBugle))
import Marvel.EncounterCard (genEncounterCard)
import Marvel.Entity
import Marvel.FearNoEvil.Cards qualified as Cards
import Marvel.Game.Source
import Marvel.GameValue
import Marvel.Identity.Types (Field(PlayerIdentitySides),PlayerIdentitySide(..))
import Marvel.Matchers
import Marvel.Name (title)
import Marvel.Message.Lifted
import Marvel.Projection
import Marvel.Query
import Marvel.Ref
import System.Random.Shuffle (shuffleM)

campaignEnvironmentFor :: CardCode -> Bool -> CardDef
campaignEnvironmentFor code completed = (if completed then fst else snd) $ case code of
  "60121a" -> (Cards.street60205a,Cards.street60205b)
  "60128a" -> (Cards.street60206a,Cards.street60206b)
  "60134a" -> (Cards.street60207a,Cards.street60207b)
  "60142a" -> (Cards.street60208a,Cards.street60208b)
  _ -> (Cards.street60209a,Cards.street60209b)

runFearNoEvilStep :: ReverseQueue m => Campaign -> CampaignStepKind -> m ()
runFearNoEvilStep c = \case
  CampaignChoices | isJust c.campaignLog.fearNoEvil.selectedScenario ->
    recordInCampaignLog $ RecordFearNoEvilLog c.campaignLog.fearNoEvil {pendingRemoved=[]}
  CampaignChoices -> do
    first <- selectJust FirstPlayer
    if c.campaignLog.fearNoEvil.gamesPlayed == 0 then chooseOnePrompted first "$label.fneVillainOrder"
      [labeled "$label.fneRecommendedVillains" (recordInCampaignLog (RecordFearNoEvilLog c.campaignLog.fearNoEvil {recommendedOrder=True}) >> campaignStep (CampaignSetupThen "fearNoEvil:planning"))
      ,labeled "$label.fneRandomVillains" (recordInCampaignLog (RecordFearNoEvilLog c.campaignLog.fearNoEvil {recommendedOrder=False}) >> campaignStep (CampaignSetupThen "fearNoEvil:planning"))]
    else campaignStep $ CampaignSetupThen "fearNoEvil:planning"
  CampaignSetupThen "fearNoEvil:planning" -> do
    remaining <- shuffleM $ fearNoEvilUnresolved c
    let chosen = if length remaining == 1 then remaining <> remaining else take 2 remaining
        advanced = progressFearNoEvil chosen c.campaignLog.fearNoEvil
        pending = [code | code <- fearNoEvilScenarioPool,code `notElem` map fst advanced.resolved]
        next = if null pending then ["60161a"] else pending
    recordInCampaignLog $ RecordFearNoEvilLog advanced {pendingRemoved=[]}
    first <- selectJust FirstPlayer
    chooseOnePrompted first "$label.fneChooseCampaignScenario"
      [cardLabeled code (cardSpecific GameRef "chooseFearNoEvilScenario" code) | code <- next]
  CampaignSetup -> do
    first <- selectJust FirstPlayer
    for_ c.campaignLog.fearNoEvil.resolved $ \(code,completed) -> genEncounterCard (campaignEnvironmentFor code completed) >>= putEncounterCardIntoPlay first
    when (List.lookup "60151a" c.campaignLog.fearNoEvil.resolved == Just True) $ do
      cards <- traverse genEncounterCard [Cards.street60153,Cards.street60154,Cards.street60155,Cards.street60156]
      setAside $ map EncounterCard cards
  CampaignSetupThen "startingHands" -> do
    when (isExpertCampaign c) $ do
      players <- getPlayers
      for_ players $ \you -> do
        code <- getIdentityCode you
        for_ (campaignPlayer code c >>= (.hitPoints)) $ setHitPoints you
      campaignStep $ CampaignSetupThen "fearNoEvil:heal"
    campaignStep $ CampaignSetupThen "fearNoEvil:environments"
    campaignStep $ CampaignSetupThen "fearNoEvil:mary"
    campaignStep $ CampaignSetupThen "fearNoEvil:scenarioSetup"
  CampaignSetupThen "fearNoEvil:heal" -> do
    players <- getPlayers
    for_ players $ \you -> do
      (hp,full,_) <- getRemainingHitPoints you
      when (hp < full) $ do
        sides <- project PlayerIdentitySides you
        recovery <- maybe (pure 0) (AE.getModifiedRecovery . toAttrs) $ headMay [ae | AlterEgoSide ae <- HashMap.elems sides]
        let restore = dealEncounterCard you >> healFrom GameRef you recovery
        if hp == 0 then restore else chooseOnePrompted you "$label.expertCampaignHeal"
          [labeled "$label.fneEncounterToHealRecovery" restore,labeled "$label.keepHitPoints" (pure ())]
  CampaignSetupThen "fearNoEvil:environments" -> do
    environments <- selectList AnyEnvironment
    traverse_ (\eid -> cardSpecific eid "campaignEnvironmentSetup" ()) environments
  CampaignSetupThen "fearNoEvil:mary" -> when (c.campaignLog.fearNoEvil.trustEstablished && not c.campaignLog.fearNoEvil.maryDefeated) $ do
    players <- getPlayers
    first <- selectJust FirstPlayer
    card <- genEncounterCard Cards.street60210a
    chooseOne first [targeting you (putEncounterCardIntoPlay you card) | you <- players]
  CampaignSetupThen "fearNoEvil:scenarioSetup" -> do
    expert <- (== Expert) <$> getDifficulty
    let log = c.campaignLog.fearNoEvil
        code = fromMaybe "60161a" log.selectedScenario
        marks = fromMaybe 0 $ List.lookup code log.progress
    case code of
      "60134a" -> placeThreatOnEach GameRef (marks * if expert then 2 else 1) EncounterMainScheme
      "60121a" -> do
        amount <- fromIntegral <$> fromGameValue (PerPlayer $ fromIntegral marks * if expert then 2 else 1)
        placeThreatOnEach GameRef amount EncounterMainScheme
      "60128a" -> when (marks > 0) $ do
        first <- selectJust FirstPlayer
        searchEverywhereAndRevealEncounterCard first $ CardIs Cards.street60131
        when (marks == 2) $ cardSpecific GameRef "fneTankerCampaignThreat" expert
      "60142a" -> when (marks > 0) $ do
        prisoners <- selectList $ MinionWithTrait Prisoner
        traverse_ (`giveStatus` ToughStatus) prisoners
        when (marks == 2) $ traverse_ dealBoost prisoners
      "60151a" -> do
        supports <- selectList $ SupportWithTrait DailyBugle
        traverse_ (`removeCounters` marks) supports
      _ -> do
        let completed = length [() | (_,True) <- log.resolved]
        when (completed >= 3) $ selectList AnyMinion >>= traverse_ (`giveStatus` ToughStatus)
        when (completed >= 4) $ selectJust FirstPlayer >>= \you -> searchEverywhereAndRevealEncounterCard you (CardIs Cards.street60165)
  CampaignVictory -> do
    recordHealth c
    removed <- fearNoEvilRemovedTitles
    psyche <- selectOne $ EnvironmentWithTitle "Disturbed Psyche"
    threat <- maybe (pure 0) getCountersOn psyche
    victory <- getVictoryDisplay
    let log = c.campaignLog.fearNoEvil
    recordInCampaignLog $ RecordFearNoEvilLog log
      {removedTitles=List.nub $ log.removedTitles <> removed,trustEstablished=log.trustEstablished || threat >= 2,maryDefeated=log.maryDefeated || any ((`elem` ["60210a","60210b"]) . toCardCode) victory}
    recordInCampaignLog $ RecordFearNoEvilOutcome True
  CampaignSetupThen "fearNoEvil:defeat" -> do
    recordHealth c
    let log = c.campaignLog.fearNoEvil
    if log.selectedScenario == Just "60161a" && isExpertCampaign c then do
      let completed = [code | (code,True) <- log.resolved]
      if null completed then recordInCampaignLog RecordCampaignLost >> recordInCampaignLog (RecordFearNoEvilOutcome False) else do
        first <- selectJust FirstPlayer
        chooseOnePrompted first "$label.fneFailCompletedScenario"
          [cardLabeled (toCardCode $ campaignEnvironmentFor code True) do
            selectList (EnvironmentWithTitle $ title $ cdName $ campaignEnvironmentFor code True) >>= traverse_ flipEnvironment
            recordInCampaignLog $ RecordFearNoEvilLog log {resolved=(code,False):filter ((/= code) . fst) log.resolved}
            recordInCampaignLog $ RecordFearNoEvilOutcome False | code <- completed]
    else recordInCampaignLog $ RecordFearNoEvilOutcome False
  _ -> pure ()

recordHealth :: ReverseQueue m => Campaign -> m ()
recordHealth c = when (isExpertCampaign c) $ do
  survivors <- getPlayers
  codes <- traverse getIdentityCode survivors
  for_ c.campaignLog.players $ \player -> unless (player.identity `elem` codes) $ recordInCampaignLog (RecordHitPoints player.identity 0)
  for_ survivors $ \you -> do
    code <- getIdentityCode you
    (remaining,_,base) <- getRemainingHitPoints you
    recordInCampaignLog $ RecordHitPoints code $ min remaining base
