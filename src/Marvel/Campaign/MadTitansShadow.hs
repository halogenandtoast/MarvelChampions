-- | The Mad Titan's Shadow's Campaign Instructions, as each scenario's 'CampaignStep'
-- follows them ("docs/rules/campaign-mad-titans-shadow.md"): its place in the campaign,
-- 1 (Ebony Maw) to 5 (Loki), says which.
module Marvel.Campaign.MadTitansShadow (runTitanStep) where

import Marvel.Prelude

import Marvel.Ally.Cards qualified as Allies
import Marvel.Campaign
import Marvel.Card
import Marvel.Damage
import Marvel.Deck
import Marvel.EncounterCard (genEncounterCard)
import Marvel.Game.Source
import Marvel.Id
import Marvel.Identity.Types (Field (..))
import Marvel.Matchers
import Marvel.Message.Lifted
import Marvel.Minion.Cards qualified as Minions
import Marvel.Obligation.Cards qualified as Obligations
import Marvel.Projection
import Marvel.Query
import Marvel.Ref
import Marvel.Resource.Cards qualified as Resources
import Marvel.SideScheme.Cards qualified as SideSchemes
import Marvel.Trait (Trait (Damaged))
import Marvel.Treachery.Cards qualified as Treacheries
import Marvel.Upgrade.Cards qualified as Upgrades

runTitanStep :: (ReverseQueue m) => Campaign -> CampaignStepKind -> m ()
runTitanStep c = \case
  CampaignChoices -> do
    players <- getPlayers
    for_ (take 1 players) (`readThenContinue` briefing c)
  CampaignSetup -> setup c
  CampaignSetupThen "topHalf" -> topHalf c
  CampaignSetupThen "expert" -> expertSetup c
  CampaignSetupThen _ -> pure ()
  CampaignVictory -> do
    players <- getPlayers
    for_ (take 1 players) (`readThenContinue` victoryInstructions c)
    victory c

-- * What the players read

storyKeys :: Campaign -> (Text -> [Text]) -> [Text]
storyKeys c sections = map ("$story.mts." <>) (sections scenario)
 where
  scenario = fromMaybe "loki" $ atMay ["ebonyMaw", "towerDefense", "thanos", "hela"] (scenarioNumber c - 1)

briefing :: Campaign -> [Text]
briefing c = storyKeys c \v ->
  if scenarioNumber c == 1
    then [v <> ".intro", "campaignMode"] <> expertOnly c ["expertCampaign"] <> [v <> ".rules", v <> ".setup"]
    else [v <> ".intro", v <> ".rules", v <> ".setup"] <> expertOnly c [v <> ".expertSetup"]

victoryInstructions :: Campaign -> [Text]
victoryInstructions c = storyKeys c \v ->
  [v <> ".victory"] <> (if scenarioNumber c == 5 then ["conclusion"] else expertOnly c [v <> ".expertVictory"])

expertOnly :: Campaign -> [a] -> [a]
expertOnly c xs = if isExpertCampaign c then xs else []

-- * Setup

-- | A new copy of the campaign set's player card, the player's own.
newCard :: (MonadRandom m) => IdentityId -> CardDef -> m PlayerCard
newCard ident def = do
  cid <- getRandom
  pure $ MkPlayerCard cid def (Just ident) (Just ident)

-- | The Setup bullets in the order printed. What must see the decks as the earlier
-- bullets left them (the top half discarded, the expert hit points and the damage after
-- them) is a later step of its own.
setup :: (ReverseQueue m) => Campaign -> m ()
setup c = do
  players <- getPlayers
  first <- selectJust FirstPlayer
  let n = scenarioNumber c
      pool m = hasMark m c
      reveal def = revealEncounterCardWithoutWindows first =<< genEncounterCard def
      intoEncounterDeck def = shuffleIntoEncounterDeck . pure =<< genEncounterCard def
      eachShuffles def = for_ players \p -> shuffleIntoDeck p . pure =<< newCard p def
  -- Open the Dungeons' Captive allies, set aside before it is in play
  when (n == 5) do
    captives <- for [Allies.ladySifCaptive, Allies.fandral, Allies.hogun, Allies.volstagg] \def -> do
      cid <- getRandom
      pure $ PlayerCard (MkPlayerCard cid def Nothing Nothing)
    setAside captives
  reveal case n of
    1 -> SideSchemes.secureTheLandingPad
    2 -> SideSchemes.saveTheShawarmaPlace
    3 -> SideSchemes.hackSanctuarysComputer
    4 -> SideSchemes.findTheNornStones
    _ -> SideSchemes.openTheDungeons
  when (n == 1 || (n `elem` [2, 3] && pool SecurityBreachInPool)) $ intoEncounterDeck SideSchemes.securityBreach
  when (n == 3 && pool CosmoInPool) $ putIntoPlay first =<< newCard first Allies.cosmoCampaign
  when (n >= 4) $ intoEncounterDeck Treacheries.summonedBack
  when (n >= 3 && pool ShawarmaInPool) $ eachShuffles Resources.shawarma
  when (n == 3 && pool BlackSwanInPool) $ putEncounterCardIntoPlay first =<< genEncounterCard Minions.blackSwan
  when (n >= 4 && pool SystemShockInPool) $ eachShuffles Obligations.systemShock
  when (n >= 4 && pool InfinityStones1BCompleted) $ campaignStep (CampaignSetupThen "topHalf")
  when (n == 5) do
    when (pool NornStoneInPool) $ for_ players \p -> putIntoPlay p =<< newCard p Upgrades.nornStoneSetup
    when (pool OdinInPool) $ putIntoPlay first =<< newCard first Allies.odinKing
  campaignStep (CampaignSetupThen "expert")

-- | "Each player discards the top half of their deck", rounded down, the deck as the
-- Shawarma and System Shock shuffled into it left it.
topHalf :: (ReverseQueue m) => Campaign -> m ()
topHalf _ = do
  players <- getPlayers
  for_ players \p -> do
    deck <- projectMap PlayerIdentityDeck unDeck p
    let half = length deck `div` 2
    when (half > 0) $ discardTopOfDeck p (fromIntegral half)

-- | Scenarios 2 to 5, expert: each player's hit points as recorded, and one acceleration
-- token on a main scheme (theirs to choose of Tower Defense's two) to heal to full; a
-- player defeated as the last scenario was won places it, unasked. Then Thanos's
-- Avengers Tower damage, printed after them, for every campaign.
expertSetup :: (ReverseQueue m) => Campaign -> m ()
expertSetup c = do
  players <- getPlayers
  when (isExpertCampaign c && scenarioNumber c > 1) do
    mainSchemes <- selectList MainScheme
    for_ players \ident -> do
      code <- getIdentityCode ident
      (_, full, _) <- getRemainingHitPoints ident
      for_ ((.hitPoints) =<< campaignPlayer code c) \hp -> do
        let healOn = [targeting (schemeRef s) (placeAccelerationToken m >> setHitPoints ident full) | s@(SchemeMainSchemeId m) <- mainSchemes]
        if hp == 0
          then case mainSchemes of
            [SchemeMainSchemeId m] -> placeAccelerationToken m >> setHitPoints ident full
            _ -> chooseOnePrompted ident "$label.healWithAccelerationToken" healOn
          else do
            setHitPoints ident hp
            when (hp < full) $
              chooseOnePrompted ident "$label.healWithAccelerationToken" $
                healOn <> [labeled "$label.keepHitPoints" (pure ())]
  when (scenarioNumber c == 3 && hasMark AvengersTowerDamaged c) $
    for_ players \p -> damageIdentity p GameRef (toDamage 3 FromAbility)

-- * Victory

victory :: (ReverseQueue m) => Campaign -> m ()
victory c = do
  survivors <- getPlayers
  codes <- traverse getIdentityCode survivors
  display <- map toCardCode <$> getVictoryDisplay
  sideSchemes <- map fst <$> getSideSchemesInPlay
  schemes <- selectList MainScheme
  mainSchemes <- map toBaseCardCode . catMaybes <$> traverse getMainSchemeCode [m | SchemeMainSchemeId m <- schemes]
  let n = scenarioNumber c
      mark m = recordInCampaignLog (RecordTitanMark m)
      defeated def = cdCardCode def `notElem` sideSchemes
      inDisplay def = cdCardCode def `elem` display
  case n of
    1 -> do
      when (defeated SideSchemes.secureTheLandingPad) $ mark CosmoInPool
      -- Attack on Knowhere (1B) completed: The Power Stone in its place
      when ("21075" `elem` mainSchemes) $ mark SecurityBreachInPool
    2 -> do
      when (defeated SideSchemes.saveTheShawarmaPlace) $ mark ShawarmaInPool
      unless (inDisplay Minions.blackSwan) $ mark BlackSwanInPool
      damaged <- selectAny (EnvironmentWithTrait Damaged)
      when damaged $ mark AvengersTowerDamaged
    3 -> do
      unless (inDisplay SideSchemes.defensiveProtocols) $ mark SystemShockInPool
      -- The Infinity Stones (1B) completed: Balance the Scales in its place
      when ("21115" `elem` mainSchemes) $ mark InfinityStones1BCompleted
    4 -> do
      when (defeated SideSchemes.findTheNornStones) $ mark NornStoneInPool
      when (inDisplay SideSchemes.retrieveOdinsArmor) $ mark OdinInPool
    _ -> pure ()
  -- a defeated player's hit points are recorded as 0: they rejoin with a token
  when (isExpertCampaign c && n < 5) do
    for_ (zip survivors codes) \(ident, code) -> do
      (remaining, _, printed) <- getRemainingHitPoints ident
      recordInCampaignLog (RecordHitPoints code (min remaining printed))
    for_ c.campaignLog.players \p ->
      unless (p.identity `elem` codes) $ recordInCampaignLog (RecordHitPoints p.identity 0)
  recordInCampaignLog RecordScenarioWon
