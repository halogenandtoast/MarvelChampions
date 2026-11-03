module Marvel.Campaign.Shadowland (runShadowlandStep) where

import Data.List qualified as List
import Marvel.Prelude
import Marvel.Campaign
import Marvel.Card
import Marvel.Deck (unDeck)
import Marvel.EncounterCard (genEncounterCard,gatherEncounterSet)
import Marvel.EncounterSet qualified as Set
import Marvel.Game.Source
import Marvel.Id
import Marvel.Identity.Types (Field(PlayerIdentityDeck))
import Marvel.Name (title)
import Marvel.Matchers
import Marvel.Message.Lifted
import Marvel.Projection
import Marvel.Query
import Marvel.Ref
import Marvel.Shadowland.Cards qualified as Cards
import Marvel.Shadowland.Identity (heroWithTitleInGame)
import Marvel.Shadowland.Corruption
import System.Random.Shuffle (shuffleM)

runShadowlandStep :: ReverseQueue m => Campaign -> CampaignStepKind -> m ()
runShadowlandStep c = \case
  CampaignChoices -> selectJust FirstPlayer >>= \you -> readThenContinue you ["$story.shadowland.setup"]
  CampaignSetup -> do
    players <- getPlayers
    let number = scenarioNumber c
    when (number == 1) $ for_ [Cards.street65013a,Cards.street65014a] $ \def ->
      whenM (heroWithTitleInGame $ title $ cdName def) $
        recordInCampaignLog $ RecordShadowlandProtege $ toCardCode def
    when (number < 3) do
      shuffleIntoEncounterDeck =<< gatherEncounterSet Set.SlBlackFlower
      cards <- traverse (const $ genEncounterCard Cards.street65072a) [1..4 :: Int]
      for_ (zip players cards) $ \(you,card) -> putEncounterCardIntoPlay you card
      setAside $ map EncounterCard $ drop (length players) cards
    -- This continuation sees the freshly put-into-play corruption upgrades.
    campaignStep $ CampaignSetupThen "shadowland:prepared"
  CampaignSetupThen "shadowland:prepared" -> do
    players <- getPlayers
    for_ players $ \you -> do
      code <- getIdentityCode you
      let history = fromMaybe 0 $ List.lookup code c.campaignLog.shadowland.overpoweringHistory
          counters = if scenarioNumber c == 2 then min 1 history else if scenarioNumber c == 3 then history else 0
      corrupt you counters
      when (scenarioNumber c > 1) $ for_ (fromMaybe [] $ List.lookup code c.campaignLog.shadowland.preparedCards) $ \reward -> do
        deck <- unDeck <$> project PlayerIdentityDeck you
        for_ (headMay $ filter ((==reward) . toCardCode) deck) $ \card ->
          chooseOnePrompted you "$label.slCampaignPrepared"
            [cardLabeled reward (putIntoPlay you card >> corrupt you 1),labeled "$label.slSkipPrepared" (pure ())]
    case scenarioNumber c of
      1 -> selectOne EncounterMainScheme >>= traverse_ (\mid -> fromIntegral <$> getPlayerCount >>= placeThreatOn GameRef mid)
      2 -> selectOne EncounterMainScheme >>= traverse_ (\mid -> (3 *) . fromIntegral <$> getPlayerCount >>= placeThreatOn GameRef mid)
      _ -> do
        cards <- shuffleM =<< traverse genEncounterCard [Cards.street65020,Cards.street65021,Cards.street65022,Cards.street65023]
        for_ (zip players cards) $ \(you,card) -> dealFoundEncounterCard you card
        shuffleIntoEncounterDeck $ drop (length players) cards
    expertSetup c
  CampaignVictory -> do
    survivors <- getPlayers
    when (scenarioNumber c == 1) $ selectOne AnyVillain >>= traverse_ (\vid -> do
      card <- getsGame (`cardInPlay` VillainRef vid)
      for_ card $ recordInCampaignLog . RecordShadowlandProtege . toCardCode)
    codes <- traverse getIdentityCode survivors
    when (isExpertCampaign c) do
      for_ c.campaignLog.players $ \p -> unless (p.identity `elem` codes) $ recordInCampaignLog (RecordHitPoints p.identity 0)
    for_ survivors $ \you -> do
      code <- getIdentityCode you
      whenM (overpowered you) $ recordInCampaignLog (RecordShadowlandOverpowered code)
      when (isExpertCampaign c) do
        (remaining,_,printed) <- getRemainingHitPoints you
        recordInCampaignLog (RecordHitPoints code $ min remaining printed)
      when (scenarioNumber c < 3) $ choosePrepared you code
    recordInCampaignLog RecordScenarioWon
  _ -> pure ()

expertSetup :: ReverseQueue m => Campaign -> m ()
expertSetup c = when (isExpertCampaign c && scenarioNumber c > 1) $ getPlayers >>= traverse_ (\you -> do
  code <- getIdentityCode you
  (_,full,_) <- getRemainingHitPoints you
  for_ (campaignPlayer code c >>= (.hitPoints)) $ \hp -> do
    let restore = dealEncounterCard you >> setHitPoints you full
    if hp == 0 then restore else do
      setHitPoints you hp
      when (hp < full) $ chooseOnePrompted you "$label.expertCampaignHeal"
        [labeled "$label.slEncounterToHeal" restore,labeled "$label.keepHitPoints" (pure ())])

choosePrepared :: ReverseQueue m => IdentityId -> CardCode -> m ()
choosePrepared you code = do
  upgrades <- map UpgradeRef <$> selectList (UpgradeControlledBy $ IdentityWithId you)
  supports <- map SupportRef <$> selectList (SupportControlledBy $ IdentityWithId you)
  cards <- for (upgrades <> supports) $ \target -> do
    card <- getsGame (`cardInPlay` target)
    pure (target,card)
  let eligible = [(target,card) | (target,Just card) <- cards,Just cost <- [cdCost $ getCardDef card],cost <= 2,cost >= 0]
  unless (null eligible) $ chooseOnePrompted you "$label.slRecordPrepared" $
    [targeting target (recordInCampaignLog $ RecordShadowlandPreparedCard code $ toCardCode card) | (target,card) <- eligible]
      <> [labeled "$label.slSkipPrepared" (pure ())]
