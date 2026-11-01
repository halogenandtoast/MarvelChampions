-- | The Galaxy's Most Wanted's Campaign Instructions, as each scenario's 'CampaignStep'
-- follows them ("docs/rules/campaign-galaxys-most-wanted.md"): its place in the campaign,
-- 1 (Brotherhood of Badoon) to 5 (Ronan the Accuser), says which.
module Marvel.Campaign.GalaxysMostWanted (runGalaxyStep) where

import Marvel.Prelude

import Data.HashMap.Strict qualified as HashMap
import Data.List qualified as List
import Marvel.Campaign
import Marvel.Card
import Marvel.Deck
import Marvel.EncounterCard (allEncounterCards, genEncounterCard)
import Marvel.EncounterSet (EncounterSet (GalacticArtifacts))
import Marvel.Game.Source
import Marvel.GameValue
import Marvel.Id
import Marvel.Identity.Types (Field (..))
import Marvel.Matchers
import Marvel.Message.Lifted
import Marvel.Projection
import Marvel.Count
import Marvel.Query
import Marvel.Ref
import Marvel.SideScheme.Cards qualified as SideSchemes
import Marvel.Treachery.Cards qualified as Treacheries
import Marvel.Hand
import Marvel.Environment.Environments.NebulasShip (getEvasionCounters, placeEvasionCounters)
import Marvel.Status ()
import Marvel.Trait (Trait (Technique))

runGalaxyStep :: (ReverseQueue m) => Campaign -> CampaignStepKind -> m ()
runGalaxyStep c = \case
  CampaignChoices -> do
    players <- getPlayers
    for_ (take 1 players) (`readThenContinue` briefing c)
  CampaignSetup -> setup c
  CampaignSetupThen "pincerManeuver" -> pincerThreat c
  CampaignSetupThen "startingHands" -> startingHands c
  CampaignSetupThen _ -> pure ()
  CampaignVictory -> do
    players <- getPlayers
    for_ (take 1 players) (`readThenContinue` victoryInstructions c)
    victory c

-- * What the players read

storyKeys :: Campaign -> (Text -> [Text]) -> [Text]
storyKeys c sections = map ("$story.galaxy." <>) (sections scenario)
 where
  scenario = fromMaybe "ronan" $ atMay ["badoon", "infiltrate", "escape", "nebula"] (scenarioNumber c - 1)

briefing :: Campaign -> [Text]
briefing c = storyKeys c \v ->
  [v <> ".intro"]
    <> (if n == 1 then ["campaignMode", "keywords", "market"] <> expertOnly c ["expertCampaign"] else [])
    <> [v <> ".rules" | n `elem` [2, 3]]
    <> [v <> ".setup"]
    <> expertOnly c [v <> ".expertSetup"]
 where
  n = scenarioNumber c

victoryInstructions :: Campaign -> [Text]
victoryInstructions c = storyKeys c \v ->
  [v <> ".victory"] <> (if scenarioNumber c == 5 then ["conclusion"] else expertOnly c [v <> ".expertVictory"])

expertOnly :: Campaign -> [a] -> [a]
expertOnly c xs = if isExpertCampaign c then xs else []

-- * Setup

-- | The scenario's Challenge side scheme: its A side in a standard campaign, B in an expert.
challenge :: Campaign -> Maybe CardDef
challenge c = atMay (if isExpertCampaign c then expert else standard) (scenarioNumber c - 1)
 where
  standard = [SideSchemes.badoonBlitz, SideSchemes.galleryOfSplendor, SideSchemes.thereIsNoEscape, SideSchemes.guerrillaTactics, SideSchemes.kreeSupremacy]
  expert = [SideSchemes.badoonBlitzExpert, SideSchemes.galleryOfSplendorExpert, SideSchemes.thereIsNoEscapeExpert, SideSchemes.guerrillaTacticsExpert, SideSchemes.kreeSupremacyExpert]

-- | Badoon Headhunter, then by the marks in "Headhunter Defeated?": On the Hunt (1), Dead
-- to Rights (2), Headhunter's Henchman (3), Fugitive Recovery (4), as many as the
-- scenario's instructions name.
headhunterCards :: Campaign -> [CardCode]
headhunterCards c = "16183" : take (min marks (scenarioNumber c - 1)) ["16184", "16185", "16186", "16187"]
 where
  marks = length c.campaignLog.galaxy.headhunterDefeated

setup :: (ReverseQueue m) => Campaign -> m ()
setup c = do
  players <- getPlayers
  first <- selectJust FirstPlayer
  let n = scenarioNumber c
      reveal def = revealEncounterCardWithoutWindows first =<< genEncounterCard def
  -- the scenario's Challenge side scheme, Kree Supremacy if the players choose to
  for_ (challenge c) \def ->
    if n == 5
      then chooseOnePrompted first "$label.revealKreeSupremacy" [cardLabeled (cdCardCode def) (reveal def), labeled "$label.pass" (pure ())]
      else reveal def
  -- Scenario 4: each Galactic Artifacts side scheme recorded, with what it brings
  when (n == 4) $ for_ (mapMaybe (`HashMap.lookup` allEncounterCards) c.campaignLog.galaxy.galacticArtifacts) \def -> do
    shuffleIntoEncounterDeck . pure =<< genEncounterCard def
    villain <- selectOne ActiveVillain
    case cdCardCode def of
      -- Hujahdarian Monarch Egg
      "16127" -> placeEvasionCounters 1
      -- Magical Teapot
      "16128" -> dealEncounterCard first
      -- Philosopher's Stone
      "16129" -> for_ villain dealBoost
      -- Crystal Ball
      "16130" -> for_ villain (`giveStatus` ToughStatus)
      _ -> pure ()
  -- Scenario 5: a player recorded in Power Stone Control is dealt "You Stand Accused!",
  -- and Pincer Maneuver is revealed with 3 - (evasion counters) per player more threat
  when (n == 5) do
    for_ c.campaignLog.galaxy.powerStoneControl \code -> do
      codes <- traverse getIdentityCode players
      for_ (List.lookup code (zip codes players)) \ident ->
        searchAndDealEncounterCard ident (CardIs Treacheries.youStandAccused)
    searchAndRevealEncounterCard first (CardIs SideSchemes.pincerManeuver)
    campaignStep (CampaignSetupThen "pincerManeuver")
  -- Badoon Headhunter, and what each mark in "Headhunter Defeated?" brings
  headhunters <- traverse genEncounterCard $ mapMaybe (`HashMap.lookup` allEncounterCards) (headhunterCards c)
  shuffleIntoEncounterDeck headhunters
  -- Scenario 3: each player's cards recorded in The Collection are removed from the game
  when (n == 3) $ for_ players \ident -> do
    code <- getIdentityCode ident
    deck <- projectMap PlayerIdentityDeck unDeck ident
    let recorded = foldMap (.collection) (galaxyPlayer code c)
        removed = foldl' (\(left, out) r -> case List.partition ((== r) . toCardCode) left of
                            (x : xs, others) -> (xs <> others, x : out)
                            ([], _) -> (left, out)) (deck, []) recorded
    for_ (snd removed) (removeFromGame . CardIdRef . pcCardId)
  when (isExpertCampaign c) do
    -- persistent damage: as recorded, a defeated player rejoining healed, any other may
    -- spend a unit to heal (scenarios 2 to 5)
    when (n > 1) $ for_ players \ident -> do
      code <- getIdentityCode ident
      (_, full, _) <- getRemainingHitPoints ident
      for_ (campaignPlayer code c) \p -> case p.hitPoints of
        Just 0 -> setHitPoints ident full
        Just hp -> do
          setHitPoints ident hp
          when (hp < full && maybe False ((> 0) . (.units)) (galaxyPlayer code c)) $
            chooseOnePrompted ident "$label.spendUnitToHeal"
              [ labeled "$label.spendUnitToHeal" do
                  recordInCampaignLog (SpendUnits code 1)
                  setHitPoints ident full
              , labeled "$label.keepHitPoints" (pure ())
              ]
        Nothing -> pure ()
    case n of
      -- each player in turn discards encounter cards until a minion, engaged with them
      1 -> for_ players \ident -> discardUntil FromEncounterDeck (CardWithType MinionType) (IdentityRef ident)
      -- each player in turn discards encounter cards until an attachment, revealed
      3 -> for_ players \ident -> discardUntil FromEncounterDeck (CardWithType AttachmentType) (IdentityRef ident)
      -- the encounter deck discarded from until a Technique attachment, attached to Nebula
      4 -> selectOne ActiveVillain >>= traverse_ \vid -> discardUntil FromEncounterDeck (CardWithType AttachmentType <> CardWithTrait Technique) (VillainRef vid)
      5 -> do
        scheme <- selectJust MainScheme
        placeThreat scheme =<< perPlayer 1
      _ -> pure ()

-- | "Place X[per_hero] additional threat on Pincer Maneuver, where X is equal to 3 minus
-- the recorded number in the Evasion Counters section."
pincerThreat :: (ReverseQueue m) => Campaign -> m ()
pincerThreat c = do
  pincers <- selectList (SideSchemeIs SideSchemes.pincerManeuver)
  let x = 3 - min 3 (fromMaybe 0 c.campaignLog.galaxy.evasionCounters)
  when (x > 0) $ for_ (take 1 pincers) \s -> placeThreat s =<< perPlayer x

-- | Infiltrate the Museum, expert: "In player order, each player must choose 1 card from
-- their hand and put it faceup into The Collection", once their starting hands are dealt.
startingHands :: (ReverseQueue m) => Campaign -> m ()
startingHands c = when (scenarioNumber c == 2 && isExpertCampaign c) do
  players <- getPlayers
  for_ players \p -> do
    hand <- projectMap PlayerIdentityHand unHand p
    chooseOnePrompted p "$label.chooseCardForCollection" [targeting card (placeInCollection card) | card <- hand]

-- | X[per_hero]: X for each player who started the scenario.
perPlayer :: (HasGame m) => Natural -> m Natural
perPlayer x = fromIntegral <$> fromGameValue (PerPlayer (fromIntegral x))

-- * Victory

victory :: (ReverseQueue m) => Campaign -> m ()
victory c = do
  survivors <- getPlayers
  codes <- traverse getIdentityCode survivors
  let n = scenarioNumber c
      -- in an expert campaign a defeated player takes no part in the victory steps
      participants
        | isExpertCampaign c = codes
        | otherwise = map (.identity) c.campaignLog.players
  display <- getVictoryDisplay
  when (n < 5) do
    points <- getVictoryPoints
    noMinions <- selectNone AnyMinion
    mainScheme <- headMay <$> (getMainSchemesIn . areaId =<< getYourArea)
    -- stage 1B: Terrestrial Invasion (1), The Art of Evasion (4)
    stageOne <- maybe (pure False) (fmap (maybe False ((`elem` ["16061", "16091"]) . toBaseCardCode)) . getMainSchemeCode) mainScheme
    noThreat <- (== 0) <$> selectCount SchemeThreat MainScheme
    collection <- getCollection
    evasion <- getEvasionCounters
    let artifacts = [cdCardCode d | d <- map getCardDef display, cdEncounterSet d == Just GalacticArtifacts, cdCardType d == SideSchemeType]
        bonus = case n of
          1 -> length (filter id [noMinions, stageOne])
          2 -> length (filter id [length collection <= length c.campaignLog.players, noThreat])
          3 -> length artifacts `div` 2
          _ -> length (filter id [evasion <= 1, stageOne])
        units = 1 + fromIntegral (max 0 (min 3 points)) + fromIntegral bonus
    for_ participants \code -> recordInCampaignLog (RecordUnits code units)
    -- Infiltrate the Museum: each player's cards in The Collection, by title
    when (n == 2) $ for_ (zip survivors codes) \(ident, code) ->
      let theirs = [toCardCode card | PlayerCard card <- collection, pcOwner card == Just ident]
       in unless (null theirs) $ recordInCampaignLog (RecordCollection code theirs)
    when (n == 3 && notNull artifacts) $ recordInCampaignLog (RecordGalacticArtifacts artifacts)
    when (any ((== "16183") . toCardCode) display) $ recordInCampaignLog (RecordHeadhunterDefeated n)
    when (n == 4) do
      holder <- getPowerStoneHolder
      for_ holder \case
        IdentityCharacter ident -> recordInCampaignLog . RecordPowerStoneControl =<< getIdentityCode ident
        _ -> pure ()
      recordInCampaignLog (RecordEvasionCounters evasion)
    when (isExpertCampaign c) do
      for_ (zip survivors codes) \(ident, code) -> do
        (remaining, _, printed) <- getRemainingHitPoints ident
        recordInCampaignLog (RecordHitPoints code (min remaining printed))
      for_ c.campaignLog.players \p ->
        unless (p.identity `elem` codes) $ recordInCampaignLog (RecordHitPoints p.identity 0)
  recordInCampaignLog RecordScenarioWon
 where
  notNull = not . null
