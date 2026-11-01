-- | Mutant Genesis campaign instructions, MC32 pp.4–5,7,10,12,16,19.
module Marvel.Campaign.MutantGenesis (runMutantGenesisStep) where

import Marvel.Prelude
import Data.HashMap.Strict qualified as HashMap
import Data.List (nub, nubBy)
import Data.Function (on)
import Data.Text qualified as T
import Marvel.Campaign
import Marvel.Card
import Marvel.Deck
import Marvel.Discard ()
import Marvel.EncounterCard (allEncounterCards, genEncounterCard)
import Marvel.Game.Source
import Marvel.Id
import Marvel.Identity.Types ( Field(PlayerIdentityDeck) )
import Marvel.Matchers
import Marvel.Message.Lifted
import Marvel.PlayerCard (allPlayerCards, lookupPlayerCard)
import Marvel.Projection
import Marvel.Query
import Marvel.Ref
import System.Random.Shuffle (shuffleM)
import Text.Read (readMaybe)

runMutantGenesisStep :: ReverseQueue m => Campaign -> CampaignStepKind -> m ()
runMutantGenesisStep c = \case
  CampaignChoices -> do
    players <- getPlayers
    for_ (take 1 players) (`readThenContinue` story c "setup")
    when (scenarioNumber c == 1) $ for_ (zip [0 :: Int ..] players) \(i, _) -> later ("role:" <> tshow i)
    -- Role-building choices happen before starting hands are drawn.
    for_ (zip [0 :: Int ..] players) \(i, _) -> later ("roleBuild:" <> tshow i)
  CampaignSetup -> do
    let log = c.campaignLog.mutantGenesis
    for_ log.futurePastActive shuffleEncounter
    remaining <- shuffleM [code | code <- futurePastCards, code `notElem` (log.futurePastActive <> log.futurePastRemoved)]
    recordInCampaignLog $ RecordFuturePastDeck remaining
    first <- selectJust FirstPlayer
    when (scenarioNumber c >= 3 && log.jubileeInPlay) $ putIntoPlay first =<< newCard first "32088b"
    when (scenarioNumber c >= 3) $ traverse_ (offerCaptive first c) log.captiveAllies
    let preceding = atMay campaignSchemes (scenarioNumber c - 2)
        earned = scenarioNumber c == 1 || maybe False (`elem` log.defeatedSchemes) preceding
    when earned $ do
      players <- getPlayers
      for_ players \ident -> do
        code <- getIdentityCode ident
        for_ (mutantPlayer code c >>= (.role)) \role -> do
          available <- shuffleM $ filter (`notElem` log.roleUpgradesRemoved) (mutantRoleCards role)
          for_ (take 1 available) \upgrade -> do
            putIntoPlay ident =<< newCard ident upgrade
            recordInCampaignLog $ RecordMutantRoleUpgrade upgrade
    for_ (atMay campaignSchemes (scenarioNumber c - 1)) (reveal first)
    later "expert"
  CampaignVictory -> do
    first <- selectJust FirstPlayer
    readThenContinue first (story c "victory")
    when (scenarioNumber c < 5) do
      removed <- filter (`elem` futurePastCards) . map toCardCode <$> getVictoryDisplay
      deck <- getEncounterDeck
      discard <- getEncounterDiscard
      minions <- selectList AnyMinion
      minionCodes <- catMaybes <$> traverse (\mid -> getsGame (\g -> toCardCode <$> cardInPlay g (MinionRef mid))) minions
      schemes <- map fst <$> getSideSchemesInPlay
      attachments <- selectList (mempty :: AttachmentMatcher)
      attachmentCodes <- catMaybes <$> traverse (\aid -> getsGame (\g -> toCardCode <$> cardInPlay g (AttachmentRef aid))) attachments
      let active = nub $ filter (`elem` futurePastCards) (map toCardCode (deck <> discard) <> minionCodes <> schemes <> attachmentCodes)
      recordInCampaignLog $ RecordFuturePast active removed
      traverse_ (recordInCampaignLog . RemoveMutantRoleUpgrade) c.campaignLog.mutantGenesis.roleUpgradesInPlay
      when (scenarioNumber c == 3) do
        prisons <- selectList (SideSchemeIs $ encounterDef "32173a")
        rescue <- selectList (EnvironmentIs $ encounterDef "32173b")
        cards <- concat <$> (traverse heldCardsAt (map SideSchemeRef prisons <> map EnvironmentRef rescue))
        recordInCampaignLog $ RecordAlliesRemoved (map toCardCode cards)
      when (scenarioNumber c >= 2) $ do
        jubilee <- selectAny (AllyWithTitle "Jubilee")
        recordInCampaignLog $ RecordMutantJubilee jubilee
      when (isExpertCampaign c) do
        survivors <- getPlayers
        codes <- traverse getIdentityCode survivors
        for_ c.campaignLog.players \p ->
          unless (p.identity `elem` codes) $ recordInCampaignLog (RecordHitPoints p.identity 0)
        for_ survivors \ident -> do
          (remaining, _, printed) <- getRemainingHitPoints ident
          recordForPlayer ident (`RecordHitPoints` min remaining printed)
    recordInCampaignLog RecordScenarioWon
  CampaignSetupThen step -> case T.stripPrefix "mutant:" step of
    Just "futurePast" -> case c.campaignLog.mutantGenesis.futurePastDeck of
      code : remaining -> shuffleEncounter code >> recordInCampaignLog (RecordFuturePastDeck remaining)
      [] -> pure ()
    Just "expert" -> expertSetup c
    Just value | Just i <- suffixNumber "role:" value -> withPlayer i (chooseRole c)
    Just value | Just i <- suffixNumber "roleBuild:" value -> withPlayer i (roleBuild c)
    _ -> pure ()

campaignSchemes :: [CardCode]
campaignSchemes = ["32171a", "32172a", "32173a", "32174a", "32175a"]

futurePastCards :: [CardCode]
futurePastCards = ["32166", "32167", "32168", "32169", "32170"]

story :: Campaign -> Text -> [Text]
story c section = ["$story.mutantGenesis." <> fromMaybe "magneto" (atMay ["sabretooth", "projectWideawake", "masterMold", "mansionAttack"] (scenarioNumber c - 1)) <> "." <> section]

later :: ReverseQueue m => Text -> m ()
later = campaignStep . CampaignSetupThen . ("mutant:" <>)

suffixNumber :: Text -> Text -> Maybe Int
suffixNumber prefix value = T.stripPrefix prefix value >>= readMaybe . T.unpack

withPlayer :: ReverseQueue m => Int -> (IdentityId -> m ()) -> m ()
withPlayer i f = getPlayers >>= traverse_ f . (`atMay` i)

newCard :: MonadRandom m => IdentityId -> CardCode -> m PlayerCard
newCard ident code = do
  cid <- getRandom
  pure $ MkPlayerCard cid (lookupPlayerCard code) (Just ident) (Just ident)

encounterDef :: CardCode -> CardDef
encounterDef code = fromMaybe (error $ "Mutant Genesis campaign card not registered: " <> show code) (HashMap.lookup code allEncounterCards)

shuffleEncounter :: ReverseQueue m => CardCode -> m ()
shuffleEncounter code = shuffleIntoEncounterDeck . pure =<< genEncounterCard (encounterDef code)

reveal :: ReverseQueue m => IdentityId -> CardCode -> m ()
reveal ident code = revealEncounterCard ident =<< genEncounterCard (encounterDef code)

chooseRole :: ReverseQueue m => Campaign -> IdentityId -> m ()
chooseRole c ident = do
  code <- getIdentityCode ident
  let taken = mapMaybe (.role) c.campaignLog.mutantGenesis.mutantPlayers
  when (isNothing $ mutantPlayer code c >>= (.role)) $
    chooseOnePrompted ident "$label.mutantChooseRole"
      [labeled ("$label.mutantRole" <> tshow role) (recordInCampaignLog $ RecordMutantRole code role)
      | role <- [minBound .. maxBound], role `notElem` taken]

roleBuild :: ReverseQueue m => Campaign -> IdentityId -> m ()
roleBuild c ident = do
  code <- getIdentityCode ident
  let earned = scenarioNumber c == 1 || maybe False (`elem` c.campaignLog.mutantGenesis.defeatedSchemes) (atMay campaignSchemes (scenarioNumber c - 2))
  when earned $ for_ (mutantPlayer code c >>= (.role)) \role -> do
    deck <- projectMap PlayerIdentityDeck unDeck ident
    let candidates = nubBy ((==) `on` cdName)
          [def | def <- HashMap.elems allPlayerCards, cdAspect def `elem` map Just (mutantRoleAspects role)
          , cdName def `notElem` map (cdName . getCardDef) deck]
    for_ [EventType, UpgradeType] \kind ->
      chooseOnePrompted ident "$label.mutantRoleBuild"
        ([cardLabeled (cdCardCode def) (shuffleIntoDeck ident . pure =<< newCard ident (cdCardCode def))
         | def <- candidates, cdCardType def == kind] <> [labeled "$label.pass" (pure ())])

offerCaptive :: ReverseQueue m => IdentityId -> Campaign -> CardCode -> m ()
offerCaptive first c code = unless (code `elem` c.campaignLog.alliesRemoved) do
  players <- getPlayers
  chooseOnePrompted first "$label.mutantCaptiveDestination"
    ([targeting ident (shuffleIntoDeck ident . pure =<< newCard ident code) | ident <- players] <> [labeled "$label.pass" (pure ())])

expertSetup :: ReverseQueue m => Campaign -> m ()
expertSetup c = when (isExpertCampaign c && scenarioNumber c > 1) do
  players <- getPlayers
  for_ players \ident -> do
    code <- getIdentityCode ident
    (_, full, _) <- getRemainingHitPoints ident
    for_ (campaignPlayer code c >>= (.hitPoints)) \hp -> do
      let healToFull = addAccelerationToken >> setHitPoints ident full
      if hp == 0 then healToFull else do
        setHitPoints ident hp
        when (hp < full) $ chooseOnePrompted ident "$label.healWithAccelerationToken"
          [labeled "$label.mutantHealWithAcceleration" healToFull, labeled "$label.keepHitPoints" (pure ())]
