{-# OPTIONS_GHC -Wno-unused-imports #-}
module Marvel.Campaign.AgentsOfShield where
import Marvel.Prelude
import Control.Monad (replicateM_)
import Marvel.Entity
import Marvel.Name (title)
import Marvel.Projection
import Data.HashMap.Strict qualified as HM
import Data.List qualified as List
import Data.Text qualified as Text
import Marvel.AgentsOfShield.Board
import Marvel.AgentsOfShield.EncounterCards qualified as Cards
import Marvel.Aspect
import Marvel.Campaign
import Marvel.Card
import Marvel.Damage
import Marvel.EncounterCard (allEncounterCards, genEncounterCard)
import Marvel.Game.Source
import Marvel.Id
import Marvel.Stats (unRec)
import Marvel.Identity.Types
import Marvel.AlterEgo.Types qualified as AE
import Marvel.Matchers
import Marvel.Message.Lifted
import Marvel.PlayerCard (lookupPlayerCard, allPlayerCards)
import Marvel.Query
import Marvel.Ref
import Marvel.Trait qualified as T
import System.Random.Shuffle (shuffleM)

boardCodes :: [CardCode]
boardCodes = ["50181a","50182a","50183a"]

-- The printed campaign log, MC50 p24; zero-based means/motive/opportunity.
moleFor :: [CardCode] -> CardCode
moleFor codes = case [fromMaybe 0 $ List.elemIndex code group | (code,group) <- zip codes evidenceGroups] of
  [a,b,c] | (a,b,c) `elem` medical -> "50181a"
          | (a,b,c) `elem` surveillance -> "50182a"
          | otherwise -> "50183a"
  _ -> "50181a"
 where
  medical = [(0,0,0),(0,0,1),(0,1,0),(0,1,2),(0,2,1),(1,0,0),(1,0,1),(1,1,0),(2,0,0)]
  surveillance = [(0,1,1),(1,0,2),(1,1,1),(1,1,2),(1,2,0),(1,2,1),(2,1,1),(2,1,2),(2,2,1)]

evidenceGroups :: [[CardCode]]
evidenceGroups = [["50185","50186","50187"],["50188","50189","50190"],["50191","50192","50193"]]

save :: ReverseQueue m => ShieldLog -> m ()
save = cardSpecific GameRef "setShieldLog"

prepareEvidence :: ReverseQueue m => m ()
prepareEvidence = do
  log <- getShieldLog
  when (null log.hiddenEvidence) do
    groups <- traverse shuffleM evidenceGroups
    remaining <- shuffleM (concatMap (drop 1) groups)
    save log {hiddenEvidence = mapMaybe headMay groups, evidenceEnvelope = remaining, earnedEvidence = []}

gainEvidence :: ReverseQueue m => Natural -> m ()
gainEvidence n = do
  log <- getShieldLog
  save log {earnedEvidence = log.earnedEvidence <> take (fromIntegral n) log.evidenceEnvelope, evidenceEnvelope = drop (fromIntegral n) log.evidenceEnvelope}

runShieldStep :: ReverseQueue m => Campaign -> CampaignStepKind -> m ()
runShieldStep c = \case
  CampaignChoices -> do
    first <- selectJust FirstPlayer
    readThenContinue first (briefing c)
  CampaignSetup -> do
    prepareEvidence
    when (scenarioNumber c == 5 && null c.campaignLog.shield.hiddenEvidence && c.campaignLog.shield.evidenceVictories > 0) $
      cardSpecific GameRef "aosGainEvidence" c.campaignLog.shield.evidenceVictories
    campaignStep (CampaignSetupThen "shield:setup")
  CampaignVictory -> do
    first <- selectJust FirstPlayer
    readThenContinue first (victoryInstructions c)
    log <- getShieldLog
    members <- boardMembers
    secrets <- traverse (\t -> (,) <$> boardCode t <*> getCountersOn t) members
    let currentSecrets = [(code,n) | (Just code,n) <- secrets]
    betrayed <- fmap catMaybes $ traverse boardCode [t | t@(AttachmentRef _) <- members]
    minions <- selectList AnyMinion
    schemes <- selectList AnySideScheme
    captives <- selectList (AllyWithTitle "Rescued Captive")
    adaptoids <- filterM (selectAny . EnvironmentIs . encounterDef) ["50109","50110","50111","50112"]
    thunderbolts <- selectList (MinionWithTrait T.Thunderbolt)
    codes <- traverse (\mid -> getCardDefForTarget (MinionRef mid)) thunderbolts
    eligible <- anyM (\t -> (==0) <$> getCountersOn t) [t | t@(EnvironmentRef _) <- members]
    let result = log
          { boardSecrets = currentSecrets
          , betrayedBoard = List.nub (log.betrayedBoard <> betrayed)
          , widowRemaining = if scenarioNumber c == 1 then fromIntegral (length minions + length schemes) else log.widowRemaining
          , rescuedCaptives = if scenarioNumber c == 2 then fromIntegral (length captives) else log.rescuedCaptives
          , adaptoidEnvironments = if scenarioNumber c == 3 then adaptoids else log.adaptoidEnvironments
          , survivingThunderbolts = if scenarioNumber c == 4 then mapMaybe (fmap toCardCode) codes else log.survivingThunderbolts
          , evidenceVictories = log.evidenceVictories + if eligible then 1 else 0
          }
    save result
    when eligible $ cardSpecific GameRef "aosGainEvidence" (1 :: Natural)
    when (isExpertCampaign c && scenarioNumber c < 5) $ recordExpertHitPoints c
    recordInCampaignLog RecordScenarioWon
  CampaignSetupThen "shield:setup" -> do
    log <- getShieldLog
    if scenarioNumber c < 5 then do
      for_ boardCodes $ \code -> do
        you <- selectJust FirstPlayer
        revealEncounterCardWithoutWindows you =<< genEncounterCard (encounterDef code)
      campaignStep (CampaignSetupThen "shield:secrets")
      for_ ["50184a","50184b","50184c"] $ \code -> shuffleIntoEncounterDeck . pure =<< genEncounterCard (encounterDef code)
    else campaignStep (CampaignSetupThen "shield:secrets")
    case scenarioNumber c of
      2 -> selectList (EnvironmentWithTitle "Alert Level") >>= traverse_ (\eid -> placeCounters eid log.widowRemaining)
      3 -> do
        cells <- selectList (EnvironmentWithTitle "Holding Cell")
        count <- fromIntegral . length <$> getPlayers
        for_ (take 1 cells) $ \eid -> do
          placeCounters eid (3*count)
          removeCounters eid (log.rescuedCaptives*count)
      5 -> do
        first <- selectJust FirstPlayer
        for_ log.adaptoidEnvironments $ \code -> revealEncounterCardWithoutWindows first =<< genEncounterCard (encounterDef code)
        for_ [1..3 :: Int] $ \_ -> shuffleIntoEncounterDeck . pure =<< genEncounterCard (encounterDef "50113")
        sets <- fmap (List.nub . mapMaybe cdEncounterSet . catMaybes) $ traverse (pure . (`HM.lookup` allEncounterCards)) (filter (/= "50133") log.survivingThunderbolts)
        for_ sets $ \set -> for_ [def | def <- HM.elems allEncounterCards, cdEncounterSet def == Just set] $ \def ->
          replicateM_ (fromIntegral $ fromMaybe 1 $ cdEncounterSetQuantity def) (shuffleIntoEncounterDeck . pure =<< genEncounterCard def)
      _ -> pure ()
  CampaignSetupThen "shield:secrets" -> do
    log <- getShieldLog
    members <- boardMembers
    for_ members $ \target -> do
      codeValue <- boardCode target
      for_ codeValue $ \code -> do
        old <- getCountersOn target
        let n = if scenarioNumber c == 1 then 2 else fromMaybe 0 $ List.lookup code log.boardSecrets
        if n >= old then placeCounters target (n-old) else removeCounters target (old-n)
        when (code `elem` log.betrayedBoard) $ case target of EnvironmentRef eid -> flipBoardMember eid; _ -> pure ()
  CampaignSetupThen "startingHands" -> do
    log <- getShieldLog
    for_ log.earnedEvidence $ \code -> cardSpecific GameRef "aosEvidenceSetup" code
    when (scenarioNumber c > 1 && isExpertCampaign c) $ campaignStep (CampaignSetupThen "shield:expert")
  CampaignSetupThen "shield:expert" -> expertSetup c
  _ -> pure ()

-- MC50 pp.4–6,9,11,13,15,18–19: explain the instructions before
-- their existing engine steps run. Evidence faces remain hidden until earned.
shieldStory :: Campaign -> Text -> [Text]
shieldStory c section =
  ["$story.aos." <> fromMaybe "baronZemo" (atMay ["blackWidow", "batroc", "modok", "thunderbolts"] (scenarioNumber c - 1)) <> "." <> section]

briefing :: Campaign -> [Text]
briefing c =
  shieldStory c "intro"
  <> (if scenarioNumber c == 1 then ["$story.aos.campaignMode", "$story.aos.evidence", "$story.aos.board"] else [])
  <> ["$story.aos.expertCampaign" | scenarioNumber c == 1 && isExpertCampaign c]
  <> shieldStory c "rules"
  <> shieldStory c "setup"
  <> ["$story.aos.laterSetup" | scenarioNumber c > 1]
  <> ["$story.aos.expertSetup" | scenarioNumber c > 1 && isExpertCampaign c]
  <> ["$story.aos.baronZemo.expertDefeat" | scenarioNumber c == 5 && isExpertCampaign c]
  <> ["$story.aos.baronZemo.standardDefeat" | scenarioNumber c == 5 && not (isExpertCampaign c)]

victoryInstructions :: Campaign -> [Text]
victoryInstructions c =
  shieldStory c "victory"
  <> ["$story.aos.recordBoard" | scenarioNumber c < 5]
  <> ["$story.aos.expertVictory" | scenarioNumber c < 5 && isExpertCampaign c]

boardCode :: HasGame m => Target -> m (Maybe CardCode)
boardCode target = fmap (\def -> CardCode (unCardCode (toBaseCardCode $ toCardCode def) <> "a")) <$> getCardDefForTarget target

getCardDefForTarget :: HasGame m => Target -> m (Maybe CardDef)
getCardDefForTarget target = getsGame (`abilityCardDef` target)

getIdentityRecovery :: HasGame m => IdentityId -> m Natural
getIdentityRecovery you = do
  sides <- project PlayerIdentitySides you
  pure $ fromMaybe 0 $ headMay [unRec $ AE.alterEgoBaseRecovery $ toAttrs a | AlterEgoSide a <- HM.elems sides]


encounterDef :: CardCode -> CardDef
encounterDef code = fromMaybe (error $ "Missing S.H.I.E.L.D. campaign card " <> show code) (HM.lookup code allEncounterCards)

recordExpertHitPoints :: ReverseQueue m => Campaign -> m ()
recordExpertHitPoints c = do
  players <- getPlayers
  codes <- traverse getIdentityCode players
  for_ c.campaignLog.players $ \p -> unless (p.identity `elem` codes) $ recordInCampaignLog (RecordHitPoints p.identity 0)
  for_ players $ \you -> do
    (remaining,_,printed) <- getRemainingHitPoints you
    code <- getIdentityCode you
    recordInCampaignLog (RecordHitPoints code $ min remaining printed)

expertSetup :: ReverseQueue m => Campaign -> m ()
expertSetup c = do
  players <- getPlayers
  for_ players $ \you -> do
    code <- getIdentityCode you
    for_ (campaignPlayer code c >>= (.hitPoints)) $ \hp -> do
      setHitPoints you hp
      members <- selectRefs (EnvironmentWithTrait T.BoardMember)
      chooseOne you $ [labeled "$label.keepHitPoints" (pure ()) | hp > 0] <> [targeting target do
        placeCounters target 1
        rec <- getIdentityRecovery you
        heal you rec | target <- members]

runShieldGameStep :: ReverseQueue m => Text -> Value -> m ()
runShieldGameStep key value = case key of
  "aosPrepareEvidence" -> prepareEvidence
  "aosGainEvidence" | Success n <- fromJSON value -> gainEvidence n
  "aosMakeAccusation" -> do
    log <- getShieldLog
    first <- selectJust FirstPlayer
    let possible = [[a,b,c] | a <- evidenceGroups!!0,b <- evidenceGroups!!1,c <- evidenceGroups!!2,all (`notElem` log.earnedEvidence) [a,b,c]]
    chooseOne first [labeled ("$label.aosAccusation." <> Text.intercalate "." (map unCardCode guess)) $ do
      save log {accusation = guess}
      cardSpecific GameRef "aosCompareAccusation" () | guess <- possible]
  "aosCompareAccusation" -> do
    log <- getShieldLog
    let accused = moleFor log.accusation
        mole = moleFor log.hiddenEvidence
        wrong = accusationMistakes log.accusation log.hiddenEvidence
    addSecretsToAll wrong
    when (accused /= mole) $ cardSpecific GameRef "aosAccusedPenalty" accused
  "aosAccusedPenalty" | Success (accused :: CardCode) <- fromJSON value -> do
    members <- boardMembers
    for_ members $ \target -> do
      code <- boardCode target
      when (code == Just accused) $ placeCounters target 3
  "aosRevealMole" -> do
    log <- getShieldLog
    let mole = moleFor log.hiddenEvidence
    selectList (EnvironmentIs $ encounterDef mole) >>= traverse_ flipBoardMember
  "aosEvidenceSetup" | Success code <- fromJSON value -> evidenceSetup code
  "aosEvidenceNext" | Success (code,players :: [IdentityId],chosen :: [Text]) <- fromJSON value -> case players of
    [] -> pure ()
    you:rest -> do
      let matcher = evidenceMatcher code
          defs = List.nubBy (\a b -> canonicalCardCode a == canonicalCardCode b)
            [def | def <- HM.elems allPlayerCards, cardMatch matcher def, title (cdName def) `notElem` chosen]
          continue names = cardSpecific GameRef "aosEvidenceSearch" (code,you,rest,names)
      members <- selectRefs (EnvironmentWithTrait T.BoardMember)
      chooseOne you $ labeled "$label.done" (continue chosen) : [targeting target $ chooseOne you
        [cardLabeled (toCardCode def) do
          cid <- getRandom
          let card = MkPlayerCard cid def (Just you) (Just you)
          placeCounters target 1
          shuffleIntoDeck you [card]
          continue (chosen <> [title $ cdName def]) | def <- defs] | target <- members]
  "aosEvidenceSearch" | Success (code,you,rest :: [IdentityId],chosen :: [Text]) <- fromJSON value -> do
    chooseOne you [labeled "$label.done" (pure ()),labeled "$label.aosEvidenceSearch" do
      placeThreatOnEach GameRef 1 EncounterMainScheme
      search you (SearchIdentityDeck you AllOfDeck) (evidenceMatcher code) SearchDrawOne ShuffleBackIn]
    cardSpecific GameRef "aosEvidenceNext" (code,rest,chosen)
  _ -> pure ()

evidenceMatcher :: CardCode -> CardMatcher
evidenceMatcher code = CardWithType kind <> CardWithAspect aspect
 where
  index = fromMaybe 0 $ List.elemIndex code (concat evidenceGroups)
  aspect = [Protection,Justice,Aggression] !! (index `mod` 3)
  kind = [AllyType,UpgradeType,SupportType] !! (index `div` 3)

evidenceSetup :: ReverseQueue m => CardCode -> m ()
evidenceSetup code = do
  players <- getPlayers
  cardSpecific GameRef "aosEvidenceNext" (code,players,[] :: [Text])

-- Means, motive, opportunity AND the accused board member are four guesses.
accusationMistakes :: [CardCode] -> [CardCode] -> Natural
accusationMistakes guessed actual = fromIntegral (length [() | (a,b) <- zip guessed actual,a /= b]) + if moleFor guessed /= moleFor actual then 1 else 0
