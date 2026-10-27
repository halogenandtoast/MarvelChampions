-- | The Rise of Red Skull's Campaign Instructions, as each scenario's 'CampaignStep'
-- follows them by default ("docs/rules/campaign-rise-of-red-skull.md"). What only the
-- scenario's own cards know (which Experimental Weapons entered play) the scenario
-- records itself, answering 'CampaignStep' before passing it on.
module Marvel.Campaign.Steps (runCampaignStep) where

import Marvel.Prelude

import Data.HashMap.Strict qualified as HashMap
import Data.List qualified as List
import Data.Text qualified as T
import Marvel.Campaign
import Marvel.Campaign.GalaxysMostWanted (runGalaxyStep)
import Marvel.Card
import Marvel.Deck
import Marvel.EncounterCard (allEncounterCards, genEncounterCard)
import Marvel.Game.Source
import Marvel.Id
import Marvel.Identity.Types
import Marvel.Matchers
import Marvel.Message.Lifted
import Marvel.PlayerCard (lookupPlayerCard)
import Marvel.Projection
import Marvel.Query
import Marvel.Ref
import Marvel.Trait (Trait (Captive))

-- | The scenario's part of the Campaign Instructions, in a campaign game: the
-- instructions of its place in the campaign, 1 (Crossbones) to 5 (Red Skull).
runCampaignStep :: (ReverseQueue m) => CampaignStepKind -> m ()
runCampaignStep step = getCampaign >>= traverse_ \c -> if c.kind == GalaxysMostWanted then runGalaxyStep c step else case step of
  CampaignChoices -> do
    players <- getPlayers
    -- first the scenario's story, the rules it brings and its Campaign Instructions
    for_ (take 1 players) (`readThenContinue` briefing c)
    for_ players \ident -> do
      code <- getIdentityCode ident
      for_ (campaignPlayer code c) \p -> for_ p.pending (choose InNextGame ident code p)
  CampaignSetup -> unless (scenarioNumber c == 1) (setup c)
  CampaignVictory -> do
    players <- getPlayers
    for_ (take 1 players) (`readThenContinue` victoryInstructions c)
    victory c
  CampaignSetupThen _ -> pure ()

-- * What the players read

-- | Each section an i18n key under @story.@, the scenario's own under its villain.
storyKeys :: Campaign -> (Text -> [Text]) -> [Text]
storyKeys c sections = map ("$story." <>) (sections villain)
 where
  villain = fromMaybe "redSkull" $ atMay ["crossbones", "absorbingMan", "taskmaster", "zola"] (scenarioNumber c - 1)

-- | As the scenario begins: its story, the rules it brings, its Setup instructions.
briefing :: Campaign -> [Text]
briefing c = storyKeys c \v -> case scenarioNumber c of
  1 -> [v <> ".intro", "campaignMode", "newKeywords"] <> expertOnly c ["expertCampaign"] <> [v <> ".rules", v <> ".setup"]
  5 -> [v <> ".intro", v <> ".rules", v <> ".setup"] <> expertOnly c ["expertSetup", v <> ".expertSetup"]
  _ -> [v <> ".intro", v <> ".rules", "setup"] <> expertOnly c ["expertSetup"]

-- | As the scenario is won: its Victory instructions.
victoryInstructions :: Campaign -> [Text]
victoryInstructions c = storyKeys c \v -> [v <> ".victory"] <> expertOnly c ["expertVictory" | scenarioNumber c < 5]

expertOnly :: Campaign -> [a] -> [a]
expertOnly c xs = if isExpertCampaign c then xs else []

-- * Choices a victory leaves

-- | When a victory's choice is made: as the scenario is won, the card only written in
-- the log (the next game is dealt with it); or as the next game is set up, by a player
-- not in the game when it was won, the card shuffled into their deck then.
data Chosen = AtVictory | InNextGame
  deriving stock (Eq)

choose :: (ReverseQueue m) => Chosen -> IdentityId -> CardCode -> CampaignPlayer -> CampaignChoice -> m ()
choose chosen ident code p choice = case choice of
  ChooseTechUpgrade -> do
    cards <- traverse (newCard ident) techUpgrades
    chooseOnePrompted ident "$label.chooseTechUpgrade" [cardLabeled (toCardCode card) (added card RecordTechUpgrade) | card <- cards]
  ChooseBasicUpgrade -> do
    cards <- traverse (newCard ident) basicUpgrades
    chooseOnePrompted ident "$label.chooseBasicUpgrade" $
      [cardLabeled (toCardCode card) (added card RecordConditionUpgrade) | card <- cards]
        <> [labeled "$label.noUpgrade" done]
  MayImproveUpgrade -> case p.conditionUpgrade of
    Just basic | "a" `T.isSuffixOf` unCardCode basic -> do
      deck <- projectMap PlayerIdentityDeck unDeck ident
      improved <- newCard ident (flipCardCode basic)
      chooseOnePrompted ident "$label.improveUpgrade"
        [ cardLabeled (toCardCode improved) do
            when (chosen == InNextGame) $
              for_ (filter ((== basic) . toCardCode) deck) (removeFromGame . CardIdRef . pcCardId)
            added improved RecordConditionUpgrade
        , labeled "$label.keepBasicUpgrade" done
        ]
    _ -> done
 where
  done = recordInCampaignLog (ResolvedChoice code choice)
  added card record = do
    when (chosen == InNextGame) $ shuffleIntoDeck ident [card]
    recordInCampaignLog (record code (toCardCode card))
    done

newCard :: (MonadRandom m) => IdentityId -> CardCode -> m PlayerCard
newCard ident code = do
  cid <- getRandom
  pure $ MkPlayerCard cid (lookupPlayerCard code) (Just ident) (Just ident)

-- * Setup, scenarios 2 to 5

setup :: (ReverseQueue m) => Campaign -> m ()
setup c = do
  -- each Experimental attachment recorded, into the encounter deck
  weapons <- traverse genEncounterCard $ mapMaybe (`HashMap.lookup` allEncounterCards) c.campaignLog.experimentalWeapons
  unless (null weapons) $ shuffleIntoEncounterDeck weapons
  players <- getPlayers
  when (scenarioNumber c == 5) do
    let x = fromMaybe 0 c.campaignLog.delayCounters
        perPlayer = if isExpertCampaign c then fromIntegral (length players) else 1
    when (x > 0) do
      scheme <- selectJust MainScheme
      placeThreat scheme (x * perPlayer)
  when (isExpertCampaign c) do
    for_ players \ident -> do
      code <- getIdentityCode ident
      for_ (campaignPlayer code c) \p -> do
        (_, full, _) <- getRemainingHitPoints ident
        for_ p.hitPoints (setHitPoints ident)
        expertHeal ident code p full
    when (scenarioNumber c == 5) $ for_ players \ident -> do
      code <- getIdentityCode ident
      when (code `elem` c.campaignLog.engagedWithMinions) $ dealEncounterCard ident

-- | "Each player may add 1 random obligation from their expert campaign set to their
-- deck to heal their identity to its full hit point value." One defeated in the last
-- scenario must, to rejoin.
expertHeal :: (ReverseQueue m) => IdentityId -> CardCode -> CampaignPlayer -> Natural -> m ()
expertHeal ident code p full = do
  let left = filter (`notElem` p.obligations) expertCampaignObligations
      hurt = maybe False (< full) p.hitPoints
      take' = do
        i <- getRandomR (0, length left - 1)
        card <- newCard ident (left !! i)
        shuffleIntoDeck ident [card]
        recordInCampaignLog (RecordObligation code (toCardCode card))
        setHitPoints ident full
  when (hurt && notNull left) $
    if p.hitPoints == Just 0
      then take'
      else
        chooseOnePrompted ident "$label.expertCampaignHeal"
          [labeled "$label.takeObligationToHeal" take', labeled "$label.keepHitPoints" (pure ())]

-- * Victory

victory :: (ReverseQueue m) => Campaign -> m ()
victory c = do
  survivors <- getPlayers
  codes <- traverse getIdentityCode survivors
  let
    -- in an expert campaign a defeated player takes no part in the victory steps
    participants
      | isExpertCampaign c = codes
      | otherwise = map (.identity) c.campaignLog.players
    -- each player chooses as it is won; one not in the game then, as the next is set up
    offer choice code = case (List.lookup code (zip codes survivors), campaignPlayer code c) of
      (Just ident, Just p) -> choose AtVictory ident code p choice
      _ -> recordInCampaignLog (RecordChoice code choice)
    each' choice = for_ participants (offer choice)
  if
    | scenarioNumber c == 1 -> each' ChooseTechUpgrade
    | scenarioNumber c == 2 -> do
        recordInCampaignLog . RecordDelayCounters =<< getCountersOnMainSchemes
        each' ChooseBasicUpgrade
    -- the Captive allies a player rescued are theirs, wherever they are now
    | scenarioNumber c == 3 -> for_ (zip survivors codes) \(ident, code) -> do
        captives <- filter (member Captive . cdTraits) <$> getCardsOwnedBy ident
        when (notNull captives && code `elem` participants) $
          recordInCampaignLog (RecordRescuedAllies code (map cdCardCode captives))
    | scenarioNumber c == 4 -> do
        for_ (zip survivors codes) \(ident, code) -> do
          engaged <- selectAny $ IdentityWithId ident <> IdentityEngagedWith AnyMinion
          when engaged $ recordInCampaignLog (RecordEngagedWithMinions code)
        prison <- List.lookup hydraPrison <$> getSideSchemesInPlay
        -- the allies beneath it are removed from the campaign
        for_ prison $ recordInCampaignLog . RecordAlliesRemoved . map toCardCode
        when (isNothing prison) $ for_ (zip survivors codes) \(ident, code) -> do
          hero <- selectAny $ IdentityWithId ident <> HeroIdentity
          let basic = any (("a" `T.isSuffixOf`) . unCardCode) ((.conditionUpgrade) =<< campaignPlayer code c)
          when (hero && basic && code `elem` participants) $ offer MayImproveUpgrade code
    | otherwise -> pure ()
  when (isExpertCampaign c) do
    for_ (zip survivors codes) \(ident, code) -> do
      (remaining, _, printed) <- getRemainingHitPoints ident
      recordInCampaignLog (RecordHitPoints code (min remaining printed))
    for_ c.campaignLog.players \p ->
      unless (p.identity `elem` codes) $ recordInCampaignLog (RecordHitPoints p.identity 0)
  recordInCampaignLog RecordScenarioWon
 where
  hydraPrison = "04122"
