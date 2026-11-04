-- | MC40 campaign instructions, pp6–7,9,11,14,16,18.
module Marvel.Campaign.NextEvolution (runNextEvolutionStep) where

import Marvel.Prelude
import Data.HashMap.Strict qualified as HashMap
import Data.List qualified as List
import Data.Maybe (maybeToList)
import Data.Text qualified as T
import Marvel.Ally.Types qualified as Ally ()
import Marvel.Campaign
import Marvel.Card
import Marvel.Damage
import Marvel.EncounterCard (allEncounterCards, genEncounterCard)
import Marvel.Game.Source
import Marvel.Id
import Marvel.Matchers
import Marvel.Message.Lifted
import Marvel.PlayerCard (lookupPlayerCard)
import Marvel.Projection ()
import Marvel.Query
import Marvel.Ref
import Marvel.Trait (Trait (Morlock, Psionic))
import System.Random.Shuffle (shuffleM)
import Text.Read (readMaybe)

runNextEvolutionStep :: ReverseQueue m => Campaign -> CampaignStepKind -> m ()
runNextEvolutionStep c = \case
  CampaignChoices -> do
    first <- selectJust FirstPlayer
    readThenContinue first (briefing c)
  CampaignSetup ->
    -- The saved-Morlock searches precede the environment reward in scenario two.
    if scenarioNumber c == 2 then later ("morlockSearch:" <> tshow c.campaignLog.nextEvolution.morlocksSaved)
      else beginEnvironments c
  CampaignVictory -> do
    first <- selectJust FirstPlayer
    readThenContinue first (victoryInstructions c)
    when (scenarioNumber c < 5) do
      for_ nextEvolutionSchemes \(_, back, _) -> do
        present <- selectAny (EnvironmentIs $ encounterDef back)
        when present $ recordInCampaignLog (RecordNextEvolutionEnvironment back)
      when (scenarioNumber c == 1) do
        routed <- selectList (EnvironmentWithTitle "Routed")
        defeated <- concat <$> traverse heldCardsAt routed
        recordInCampaignLog (RecordNextEvolutionMarauders $ map toCardCode defeated)
        saved <- length <$> selectList (AllyWithTrait Morlock)
        recordInCampaignLog (RecordNextEvolutionMorlocks $ fromIntegral saved)
      when (scenarioNumber c `elem` [3,4]) do
        hope <- selectList (AllyWithTitle "Hope Summers")
        for_ (take 1 hope) \aid -> do
          damage <- characterStoredDamage (AllyCharacter aid)
          recordInCampaignLog (RecordNextEvolutionHopeDamage (scenarioNumber c) damage)
      when (isExpertCampaign c) $ recordExpertHitPoints c
    recordInCampaignLog RecordScenarioWon
  CampaignSetupThen step -> case T.stripPrefix "nextEvolution:" step of
    Just "environments" -> beginEnvironments c
    Just "afterEnvironments" -> afterEnvironments c
    Just "scenarioSetup" -> scenarioSetup c
    Just "chooseScheme" -> chooseScheme c
    Just "chosenEncounters" -> do
      for_ c.campaignLog.nextEvolution.chosenSchemes \(_, front) ->
        for_ [encounter | (code, _, encounter) <- nextEvolutionSchemes, code == front] $ \code ->
          shuffleIntoEncounterDeck . pure =<< genEncounterCard (encounterDef code)
      later "expert"
    Just "expert" -> expertSetup c
    Just "stryfeShuffle" -> shuffleEncounterDiscardIntoDeck
    Just value | Just codes <- T.stripPrefix "environments:" value ->
      chooseEnvironmentOrder (filter (/= "") $ map CardCode $ T.splitOn "," codes)
    Just value | Just n <- suffixNumber "morlockSearch:" value ->
      if n <= 0 then beginEnvironments c else do
        first <- selectJust FirstPlayer
        players <- getPlayers
        chooseOnePrompted first "$label.neMorlockSearchPlayer"
          [targeting you $ search you (SearchIdentityDeck you AllOfDeck) AnyCard SearchDrawOne ShuffleBackIn | you <- players]
        later ("morlockSearch:" <> tshow (n - 1))
    Just value | Just i <- suffixNumber "stryfeReveal:" value -> do
      players <- getPlayers
      for_ (atMay players i) \you -> do
        let matcher = CardMatchesAny [CardWithType MinionType, CardWithType AttachmentType <> CardWithTrait Psionic]
        deck <- getEncounterDeck
        let (before, matches) = break (cardMatch matcher) deck
        discardTopOfEncounterDeck (fromIntegral $ length before)
        for_ (take 1 matches) (revealEncounterCard you)
    _ -> pure ()

story :: Campaign -> Text -> [Text]
story c section = ["$story.nextEvolution." <> fromMaybe "stryfe" (atMay ["morlockSiege", "onTheRun", "juggernaut", "misterSinister"] (scenarioNumber c - 1)) <> "." <> section]

-- The story and rules are read before ordinary scenario setup; campaign setup
-- effects remain in CampaignSetup and are never performed by these readings.
briefing :: Campaign -> [Text]
briefing c = story c "intro"
  <> ["$story.nextEvolution." <> section | scenarioNumber c == 1, section <- ["campaignMode", "campaignSchemes", "playerSideSchemes", "keywords"]]
  <> ["$story.nextEvolution.expertCampaign" | scenarioNumber c == 1 && isExpertCampaign c]
  <> ["$story.nextEvolution.hopeSummers" | scenarioNumber c == 3]
  <> story c "rules"
  <> story c "setup"
  <> [key | isExpertCampaign c && scenarioNumber c > 1, key <- story c "expertSetup"]

victoryInstructions :: Campaign -> [Text]
victoryInstructions c = story c "victory"
  <> ["$story.nextEvolution.expertVictory" | isExpertCampaign c && scenarioNumber c < 5]
  <> ["$story.nextEvolution.conclusion" | scenarioNumber c == 5]

later :: ReverseQueue m => Text -> m ()
later = campaignStep . CampaignSetupThen . ("nextEvolution:" <>)

suffixNumber :: Text -> Text -> Maybe Int
suffixNumber prefix value = T.stripPrefix prefix value >>= readMaybe . T.unpack

encounterDef :: CardCode -> CardDef
encounterDef code = fromMaybe (error $ "NeXt Evolution campaign card not registered: " <> show code) (HashMap.lookup code allEncounterCards)

newCard :: MonadRandom m => IdentityId -> CardCode -> m PlayerCard
newCard you code = do
  cid <- getRandom
  pure $ MkPlayerCard cid (lookupPlayerCard code) (Just you) (Just you)

beginEnvironments :: ReverseQueue m => Campaign -> m ()
beginEnvironments c = chooseEnvironmentOrder c.campaignLog.nextEvolution.earnedEnvironments

-- The order matters for enters-play effects and is explicitly chosen by the group.
chooseEnvironmentOrder :: ReverseQueue m => [CardCode] -> m ()
chooseEnvironmentOrder [] = later "afterEnvironments"
chooseEnvironmentOrder remaining = do
  first <- selectJust FirstPlayer
  chooseOnePrompted first "$label.neEnvironmentOrder"
    [cardLabeled code do
      putEncounterCardIntoPlay first =<< genEncounterCard (encounterDef code)
      later ("environments:" <> T.intercalate "," (map unCardCode $ List.delete code remaining))
    | code <- remaining]

afterEnvironments :: ReverseQueue m => Campaign -> m ()
afterEnvironments c = do
  count <- fromIntegral . length <$> filterM (selectAny . EnvironmentIs . encounterDef) c.campaignLog.nextEvolution.earnedEnvironments
  case scenarioNumber c of
    2 -> when (count > 0) $ selectList AnyEnemy >>= traverse_ enemyBecomesTough
    3 -> do
      juggernaut <- maybeToList <$> villainNamed "Juggernaut"
      for_ juggernaut (`placeCounters` count)
    4 -> do
      first <- selectJust FirstPlayer
      putEncounterCardIntoPlay first =<< genEncounterCard (encounterDef "40146")
      later "scenarioSetup"
    5 -> do
      schemes <- selectList (SideSchemeIs $ encounterDef "40168a")
      n <- fromIntegral <$> getStartingPlayerCount
      for_ schemes \sid -> placeThreat sid (n * count)
      later "scenarioSetup"
    _ -> pure ()
  unless (scenarioNumber c `elem` [4,5]) $ later "scenarioSetup"

scenarioSetup :: ReverseQueue m => Campaign -> m ()
scenarioSetup c = do
  let log = c.campaignLog.nextEvolution
  case scenarioNumber c of
    3 -> do
      -- These actual copies leave the encounter deck before being dealt; do not
      -- generate extra copies of a modular card already in the encounter deck.
      players <- getPlayers
      deck <- getEncounterDeck
      discard <- getEncounterDiscard
      let pool = deck <> discard
          selected = take 1 (filter ((== "40132") . toCardCode) pool)
            <> take (length players) (filter ((== "40133") . toCardCode) pool)
      shuffled <- shuffleM selected
      setAside (map EncounterCard shuffled)
      for_ (zip players shuffled) (uncurry dealFoundEncounterCard)
      shuffleIntoEncounterDeck (drop (length players) shuffled)
    4 -> do
      schemes <- selectList (SideSchemeIs $ encounterDef "40146")
      n <- fromIntegral <$> getStartingPlayerCount
      let count = fromIntegral $ length log.earnedEnvironments
      for_ schemes \sid -> placeThreat sid (n * count)
      chooseHopeDamageOrThreat c "40146" 3
    5 -> do
      chooseHopeDamageOrThreat c "40168a" 4
      players <- getPlayers
      for_ (zip [0 :: Int ..] players) \(i,_) -> later ("stryfeReveal:" <> tshow i)
      later "stryfeShuffle"
    _ -> pure ()
  later "chooseScheme"

chooseHopeDamageOrThreat :: ReverseQueue m => Campaign -> CardCode -> Int -> m ()
chooseHopeDamageOrThreat c schemeCode previous = do
  let amount = fromMaybe 0 $ List.lookup previous c.campaignLog.nextEvolution.hopeDamage
  when (amount > 0) do
    first <- selectJust FirstPlayer
    hope <- selectList (AllyWithTitle "Hope Summers")
    schemes <- selectList (SideSchemeIs $ encounterDef schemeCode)
    let damage = [labeled "$label.nePlaceHopeDamage" (damageCharacter aid GameRef $ toDamage amount FromAbility) | aid <- take 1 hope]
        threat = [labeled "$label.nePlaceHopeThreat" (placeThreat sid amount) | sid <- take 1 schemes]
    unless (null $ damage <> threat) $ chooseOnePrompted first "$label.neHopeDamageOrThreat" (damage <> threat)

chooseScheme :: ReverseQueue m => Campaign -> m ()
chooseScheme c = do
  first <- selectJust FirstPlayer
  let selected = c.campaignLog.nextEvolution.chosenSchemes
      previous = List.lookup (scenarioNumber c) selected
      used = map snd selected
      available = [front | (front,_,_) <- nextEvolutionSchemes, maybe (front `notElem` used) (== front) previous]
  unless (null available) $ chooseOnePrompted first "$label.neChooseCampaignScheme"
    [cardLabeled code do
      recordInCampaignLog (RecordNextEvolutionScheme (scenarioNumber c) code)
      putIntoPlay first =<< newCard first code
      later "chosenEncounters"
    | code <- available]

expertSetup :: ReverseQueue m => Campaign -> m ()
expertSetup c = when (isExpertCampaign c && scenarioNumber c > 1) do
  players <- getPlayers
  for_ players \you -> do
    code <- getIdentityCode you
    (_, full, _) <- getRemainingHitPoints you
    for_ (campaignPlayer code c >>= (.hitPoints)) \hp -> do
      let penalty = if scenarioNumber c `elem` [2,4] then addAccelerationToken else dealEncounterCard you
          heal = penalty >> setHitPoints you full
      if hp == 0 then heal else do
        setHitPoints you hp
        when (hp < full) $ chooseOnePrompted you "$label.expertCampaignHeal"
          [labeled (if scenarioNumber c `elem` [2,4] then "$label.neAccelerationToHeal" else "$label.neEncounterToHeal") heal
          ,labeled "$label.keepHitPoints" (pure ())]

recordExpertHitPoints :: ReverseQueue m => Campaign -> m ()
recordExpertHitPoints c = do
  survivors <- getPlayers
  codes <- traverse getIdentityCode survivors
  for_ c.campaignLog.players \p -> unless (p.identity `elem` codes) $ recordInCampaignLog (RecordHitPoints p.identity 0)
  for_ survivors \you -> do
    (remaining, _, printed) <- getRemainingHitPoints you
    code <- getIdentityCode you
    recordInCampaignLog (RecordHitPoints code $ min remaining printed)
