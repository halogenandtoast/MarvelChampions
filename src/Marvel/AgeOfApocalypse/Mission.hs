module Marvel.AgeOfApocalypse.Mission
  ( runMissionStep, startingAlly, missionResources, missionAllyStats
  ) where

import Marvel.Prelude
import Control.Monad ((>=>))
import Data.HashMap.Strict qualified as HashMap
import Data.List qualified as List
import Marvel.AgeOfApocalypse.MissionTypes
import Marvel.Card
import Marvel.Damage
import Marvel.EncounterCard (allEncounterCards, genEncounterCard)
import Marvel.GameValue
import Marvel.Game.Source
import Marvel.Hero.Cards qualified as Heroes
import Marvel.Id
import Marvel.Matchers
import Marvel.Message.Lifted
import Marvel.Modifier
import Marvel.PlayerCard (lookupPlayerCard)
import Marvel.Query
import Marvel.Ref
import Marvel.Resource.Types
import Marvel.SideScheme.Cards qualified as Schemes
import Marvel.Support.Cards qualified as Supports

save :: ReverseQueue m => MissionState -> m ()
save = cardSpecific GameRef "aoaSetMission"

step :: (ReverseQueue m, ToJSON a) => Text -> a -> m ()
step = cardSpecific GameRef

encounterDef :: CardCode -> CardDef
encounterDef code = fromMaybe (error $ "Missing mission encounter " <> show code) $ HashMap.lookup code allEncounterCards

newCard :: MonadRandom m => Maybe IdentityId -> CardCode -> m PlayerCard
newCard owner code = do
  cid <- getRandom
  pure $ MkPlayerCard cid (lookupPlayerCard code) owner owner

physical :: (HasGame m, IsRef ref) => ref -> m (Maybe Card)
physical ref = getsGame (`cardInPlay` toTarget ref)

live :: HasGame m => MissionState -> m MissionState
live m = do
  allies <- filterM (fmap isJust . physical) m.missionAllies
  minions <- filterM (fmap isJust . physical) m.missionMinions
  pure m {missionAllies = allies, missionMinions = minions
         ,missionParticipants = filter (`elem` allies) m.missionParticipants}

withMission :: ReverseQueue m => (MissionState -> m ()) -> m ()
withMission f = getMission >>= traverse_ (live >=> f)

-- | Mission allies keep their printed icons and Desperate Measures adds a wild.
missionResources :: HasGame m => AllyId -> m [Resource]
missionResources aid = do
  def <- maybe (error "Mission ally missing") getCardDef <$> physical aid
  upgrades <- getAllyAttachedUpgrades aid
  upgradeDefs <- catMaybes <$> traverse (fmap (fmap getCardDef) . physical) upgrades
  pure $ printedResources def <> [Wild | any ((== "45176") . toCardCode) upgradeDefs]

missionAllyStats :: HasGame m => AllyId -> m (Natural, Natural)
missionAllyStats = getMissionAllyStats

startingAlly :: ReverseQueue m => IdentityId -> Bool -> m ()
startingAlly you expert = do
  code <- getIdentityCode you
  let traits = maybe [] (toList . cdTraits) $ HashMap.lookup code Heroes.allHeroesMap
      shared = if expert then CardMatchesAny (map CardWithTrait traits) else AnyCard
  search you (SearchIdentityDeck you AllOfDeck) (CardWithType AllyType <> shared) SearchDrawOne ShuffleBackIn

runMissionStep :: ReverseQueue m => Text -> Value -> m ()
runMissionStep key value = case (key, fromJSON value) of
  ("aoaMissionSetup", Success (code, overseer)) -> setupMission code overseer
  _ -> runOther key value

runOther :: ReverseQueue m => Text -> Value -> m ()
runOther key value = case key of
  "aoaMissionSetupFinished" -> withMission $ \m -> save m {missionSettingUp = False}
  "aoaMissionAttempt" | Success you <- fromJSON value -> withMission $ \m ->
    unless (m.missionFinished || isJust m.missionActor) do
      save m {missionActor = Just you, missionDiscarded = [], missionAssignments = [], missionParticipants = [], missionDamagePool = 0}
      discardFromDeck you (fromIntegral $ length m.missionAllies) (SideSchemeRef m.missionScheme)
  "aoaMissionDiscarded" | Success cards <- fromJSON value -> withMission $ \m -> do
    save m {missionDiscarded = cards}
    overseerResponse m cards
    step "aoaMissionAssign" ()
  "aoaMissionAssign" -> withMission assignNext
  "aoaMissionAssigned" | Success (aid, card) <- fromJSON value -> withMission $ \m -> do
    icons <- missionResources aid
    let cardIcons = printedResources $ getCardDef card
        matched = any (\a -> any (\b -> a == Wild || b == Wild || a == b) cardIcons) icons
    save m {missionDiscarded = filter ((/= pcCardId card) . pcCardId) m.missionDiscarded
           ,missionAssignments = m.missionAssignments <> [(aid,card)]
           ,missionParticipants = m.missionParticipants <> [aid | matched]}
    step "aoaMissionAssign" ()
  "aoaMissionAssignmentSkip" | Success card <- fromJSON value -> withMission $ \m -> do
    save m {missionDiscarded = filter ((/= pcCardId card) . pcCardId) m.missionDiscarded}
    step "aoaMissionAssign" ()
  "aoaMissionDamage" -> withMission damageNext
  "aoaMissionDealDamage" | Success (mid, amount) <- fromJSON value -> withMission $ \m -> do
    save m {missionDamagePool = subtractNatural amount m.missionDamagePool}
    dealDamage (mid :: MinionId) (SideSchemeRef m.missionScheme) (toDamage amount FromAbility)
    step "aoaMissionDamage" ()
  "aoaMissionThwart" -> withMission $ \m -> do
    stats <- traverse missionAllyStats m.missionParticipants
    sideSchemeStep m.missionScheme (SideSchemeThreatRemoved (SideSchemeRef m.missionScheme) $ sum $ map fst stats)
    step "aoaMissionAfterAttempt" ()
  "aoaMissionAfterAttempt" -> withMission $ \m -> unless m.missionFinished do
    let attempts = m.missionAttempts + 1
    save m {missionAttempts = attempts}
    placeCounters (SideSchemeRef m.missionScheme) 1
    for_ m.missionAllies $ \aid -> damageCharacter aid (SideSchemeRef m.missionScheme) (toDamage 1 FromAbility)
    step "aoaMissionAttemptFinished" ()
  "aoaMissionAttemptFinished" -> withMission $ \m ->
    if m.missionAttempts >= 4 then step "aoaMissionFinish" False
      else save m {missionActor = Nothing, missionAssignments = [], missionDiscarded = [], missionParticipants = []}
  "aoaMissionFinish" | Success success <- fromJSON value -> withMission $ finishMission success
  "aoaMissionFinishCleared" -> withMission $ \m -> do
    save m {missionAllies = [], missionMinions = [], missionParticipants = [], missionAssignments = [], missionDiscarded = [], missionActor = Nothing}
    missionReward m
  "aoaMissionAllyEntered" | Success aid <- fromJSON value -> withMission $ \m ->
    save m {missionAllies = List.nub (m.missionAllies <> [aid]), missionPendingAlly = Nothing}
  "aoaMissionMinionEntered" | Success mid <- fromJSON value -> withMission $ \m -> do
    save m {missionMinions = List.nub (m.missionMinions <> [mid])}
    players <- getPlayers
    for_ players $ \you -> identityStep you (MinionDisengaged mid)
  "aoaMissionDiscount" -> withMission $ \m -> save m {missionDiscount = True}
  "aoaMissionEndPhase" -> withMission $ \m -> save m {missionDiscount = False}
  "aoaMissionPrune" -> withMission save
  "aoaMissionMikhailDamage" | Success n <- fromJSON value -> withMission $ \m -> when ((n :: Natural) > 0 && not (null m.missionAllies)) do
    for_ m.missionActor $ \you -> chooseOrRunOne you
      [targeting (AllyRef aid) (damageCharacter aid (SideSchemeRef m.missionScheme) $ toDamage n FromAbility)
      | aid <- m.missionAllies]
  "aoaMissionEliminateOwner" | Success you <- fromJSON value -> withMission $ \m -> do
    upgrades <- concat <$> traverse getAllyAttachedUpgrades m.missionAllies
    let targets = map UpgradeRef upgrades <> map AllyRef m.missionAllies <> map MinionRef m.missionMinions
    owned <- filterM (\target -> do
      card <- physical target
      pure $ case card of Just (PlayerCard c) -> pcOwner c == Just (you :: IdentityId); _ -> False) targets
    held <- filter ((== Just you) . pcOwner) . concat <$> traverse getZoneHeldCards targets
    for_ held $ \card -> removePlayerCardFromHeldZones card >> discardCard card
    traverse_ removeFromPlay owned
  "aoaMissionSeaWallReward" | Success remaining <- fromJSON value -> case remaining of
    [] -> pure ()
    you : rest -> withMission $ \m -> do
      enemies <- selectList DamageableEnemy
      if null enemies then step "aoaMissionSeaWallReward" (rest :: [IdentityId])
        else chooseOne you [targeting (enemyRef enemy) do
          dealDamage enemy (SideSchemeRef m.missionScheme) (toDamage 3 FromAbility)
          step "aoaMissionSeaWallReward" (rest :: [IdentityId])
          | enemy <- enemies]
  "aoaMissionLostMutantReward" | Success remaining <- fromJSON value -> case remaining of
    [] -> pure ()
    you : rest -> do
      pool <- filter ((`elem` ["45172","45173","45174","45175"]) . toCardCode) <$> getSetAsidePlayerCards
      chooseOne you [cardLabeled (toCardCode c) do
        removeFromGame c
        takeControlOf you c
        step "aoaMissionLostMutantReward" (rest :: [IdentityId])
        | c <- pool]
  "aoaMissionCampaignAllyEnteredHand" | Success (you,card) <- fromJSON value -> campaignAllyResponse you card
  _ -> pure ()

setupMission :: ReverseQueue m => CardCode -> CardCode -> m ()
setupMission code overseer = do
  first <- selectJust FirstPlayer
  scheme <- genEncounterCard (encounterDef code)
  enemy <- genEncounterCard (encounterDef overseer)
  let sid = SideSchemeId $ unCardId $ ecCardId scheme
      mid = MinionId $ unCardId $ ecCardId enemy
      mission = (newMissionState code sid overseer) {missionMinions = [mid]}
  save mission
  putEncounterCardIntoPlay first scheme
  putEncounterCardIntoPlay first enemy
  putIntoPlay first =<< newCard (Just first) "45171a"
  when (code == "45166a") do
    players <- getPlayers
    for_ players $ \you -> setAside . pure . PlayerCard =<< newCard (Just you) "45176"
  when (code == "45169a") $
    for_ ["45172","45173","45174","45175"] $ \ally -> setAside . pure . PlayerCard =<< newCard Nothing ally
  step "aoaMissionSetupFinished" ()

-- | The response is checked against the actual Overseer still at the mission.
overseerResponse :: ReverseQueue m => MissionState -> [PlayerCard] -> m ()
overseerResponse m cards = do
  overseers <- filterM (\mid -> maybe False ((== m.missionOverseer) . toCardCode . getCardDef) <$> physical mid) m.missionMinions
  for_ overseers $ \mid -> case m.missionOverseer of
    "45180a" -> placeThreat m.missionScheme (2 * icons Mental)
    "45181a" -> do
      let wilds = filter (elem Wild . printedResources . getCardDef) cards
      for_ wilds (attachPlayerCardToMinion mid)
      save m {missionDiscarded = filter (not . (`elem` map pcCardId wilds) . pcCardId) cards}
    "45182a" -> heal mid (3 * icons Physical)
    "45183a" -> step "aoaMissionMikhailDamage" (icons Energy)
    _ -> pure ()
 where
  icons resource = fromIntegral $ length [() | card <- cards, r <- printedResources (getCardDef card), r == resource]

assignNext :: ReverseQueue m => MissionState -> m ()
assignNext m = case m.missionDiscarded of
  [] -> do
    stats <- traverse missionAllyStats m.missionParticipants
    save m {missionDamagePool = sum $ map snd stats}
    step "aoaMissionDamage" ()
  card : _ -> for_ m.missionActor $ \you -> do
    sinister <- anyM (\mid -> maybe False ((== "45179a") . toCardCode . getCardDef) <$> physical mid) m.missionMinions
    let assigned = map fst m.missionAssignments
        previousIcons = concatMap (printedResources . getCardDef . snd) m.missionAssignments
        legalResource = not sinister || null [r | r <- printedResources (getCardDef card), r `elem` previousIcons]
        available = [aid | aid <- m.missionAllies, aid `notElem` assigned, legalResource]
    focusing [PlayerCard card] $
      if null available then step "aoaMissionAssignmentSkip" card
        else chooseOnePrompted you "$label.aoaAssignMissionCard"
          ([targeting (AllyRef aid) (step "aoaMissionAssigned" (aid,card)) | aid <- available]
            <> [labeled "$label.aoaMissionSkipAssignment" (step "aoaMissionAssignmentSkip" card) | sinister])

damageNext :: ReverseQueue m => MissionState -> m ()
damageNext m
  | m.missionDamagePool == 0 || null m.missionMinions = step "aoaMissionThwart" ()
  | otherwise = for_ m.missionActor $ \you -> do
      let others = length m.missionMinions > 1
      eligible <- filterM (\mid -> do
        card <- physical mid
        pure $ not (others && maybe False ((== m.missionOverseer) . toCardCode . getCardDef) card)) m.missionMinions
      chooseOnePrompted you "$label.aoaMissionDamageTarget"
        [targeting (enemyRef $ EnemyMinionId mid) do
          hp <- getEnemyRemainingHitPoints (EnemyMinionId mid)
          if hp == 0 then step "aoaMissionThwart" ()
            else chooseOrRunOne you
              [labeled (countLabel "aoaMissionDamageAmount" amount) (step "aoaMissionDealDamage" (mid,amount))
              | amount <- [1 .. m.missionDamagePool]]
        | mid <- eligible]

finishMission :: ReverseQueue m => Bool -> MissionState -> m ()
finishMission success m = unless m.missionFinished do
  upgrades <- concat <$> traverse getAllyAttachedUpgrades m.missionAllies
  let targets = map UpgradeRef upgrades <> map AllyRef m.missionAllies <> map MinionRef m.missionMinions
  inPlay <- catMaybes <$> traverse physical targets
  held <- concat <$> traverse getZoneHeldCards targets
  let cards = List.nubBy (\a b -> pcCardId a == pcCardId b) ([c | PlayerCard c <- inPlay] <> held)
  save m {missionFinished = True, missionSucceeded = success, missionDiscount = False, missionPendingAlly = Nothing}
  -- The front's When Defeated shuffles its player cards before either flip.
  -- On failure they stay until the Finished side removes the mission area.
  when success do
    for_ cards removePlayerCardFromHeldZones
    for_ targets removeFromGame
    for_ cards removeFromGame
    players <- getPlayers
    for_ players $ \you -> shuffleIntoDeck you [c | c <- cards, pcOwner c == Just you]
  teams <- selectList (SupportNamed $ cdName Supports.missionTeamFront)
  for_ teams $ \sid -> if success then cardSpecific sid "aoaMissionTeamFlip" () else removeFromGame sid
  cardSpecific (SideSchemeRef m.missionScheme) "aoaMissionFlip" ()
  unless success do
    for_ cards removePlayerCardFromHeldZones
    for_ targets removeFromGame
    for_ cards removeFromGame
  removeFromGame (SideSchemeRef m.missionScheme)
  step "aoaMissionFinishCleared" ()


missionReward :: ReverseQueue m => MissionState -> m ()
missionReward m = do
  players <- getPlayers
  first <- selectJust FirstPlayer
  count <- fromIntegral <$> getStartingPlayerCount
  if m.missionSucceeded then case m.missionCard of
    "45166a" -> for_ players $ \you -> do
      cards <- getSetAsidePlayerCards
      for_ (headMay [c | c <- cards, toCardCode c == "45176", pcOwner c == Just you]) $ \card -> removeFromGame card >> addToHand you card
    "45167a" -> for_ players $ \you -> search you (SearchIdentityDeckAndDiscardPile you) AnyCard SearchDrawOne ShuffleBackIn
    "45168a" -> do
      wall <- selectList (SideSchemeIs Schemes.northAmericanSeaWall)
      traverse_ removeFromGame wall
      deck <- getEncounterDeck
      discard <- getEncounterDiscard
      aside <- getSetAsideEncounterCards
      for_ [c | c <- deck <> discard <> aside, toCardCode c == "45177"] removeFromGame
      step "aoaMissionSeaWallReward" players
    "45169a" -> step "aoaMissionLostMutantReward" players
    "45170a" -> for_ players $ \you -> search you (SearchIdentityDeckAndDiscardPile you) (CardWithType AllyType) SearchDrawOne ShuffleBackIn
    _ -> pure ()
  else case m.missionCard of
    "45166a" -> placeThreatOnEach (SideSchemeRef m.missionScheme) (2 * count) MainScheme
    "45167a" -> traverse_ dealEncounterCard players
    "45168a" -> do
      wall <- selectList (SideSchemeIs Schemes.northAmericanSeaWall)
      if null wall then searchEverywhereAndRevealEncounterCard first (CardIs Schemes.northAmericanSeaWall)
        else for_ wall $ \sid -> physical sid >>= traverse_ (\case EncounterCard c -> revealEncounterCard first c; _ -> pure ())
    "45169a" -> for_ players (`discardFromHand` 1)
    "45170a" -> gameOver Lost
    _ -> pure ()

campaignAllyResponse :: ReverseQueue m => IdentityId -> PlayerCard -> m ()
campaignAllyResponse you card = do
  let code = toCardCode card
  when (code `elem` ["45172","45173","45174","45175"]) do
    chooseVillain <- elem PlayersChooseTheVillain <$> getModifiers GameRef
    villains <- selectList (if chooseVillain then AnyVillain else ActiveVillain)
    damageable <- filterM (\vid -> not <$> cannotTakeDamageFrom (VillainRef vid) (toSource card)) villains
    confusable <- filterM (enemyMatches EnemyCanBeConfused . EnemyVillainId) villains
    threat <- selectAny (EncounterMainScheme <> SchemeWithThreat (GreaterThan (Static 0)))
    tough <- selectAny (CharacterWithId (IdentityCharacter you) <> CharacterCanGainTough)
    let possible = case code of
          "45172" -> threat
          "45173" -> not (null damageable)
          "45174" -> not (null confusable)
          "45175" -> tough
          _ -> False
    when possible $ chooseOne you
      [cardLabeled code $ case code of
        "45172" -> removeThreat you card 2 EncounterMainScheme
        "45173" -> chooseOrRunOne you [targeting (enemyRef $ EnemyVillainId vid) (dealDamage vid card $ toDamage 2 FromAbility) | vid <- damageable]
        "45174" -> chooseOrRunOne you [targeting (enemyRef $ EnemyVillainId vid) (confuse vid card) | vid <- confusable]
        "45175" -> giveStatus you ToughStatus
        _ -> pure ()
      ,labeled "$label.noResponse" (pure ())]
