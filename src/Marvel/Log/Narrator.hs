-- | The game log, derived from the message stream, as in Arkham's "Arkham.Log.Narrator".
--
-- Cards do not write the log. Every change to the game passes through a 'Message', so
-- one 'case' here, knowing what each message means, gives the whole log in one place a
-- reader can review, and a new card logs correctly without doing anything.
--
-- Three rules:
--
-- 1. __Silence is the default.__ Most messages are plumbing (windows, asks, payments),
--    and a message nothing here claims says nothing.
-- 2. __The narrator runs before the message.__ It reads the game as the message finds
--    it: a card about to leave play is still there to be named, a deck still holds the
--    cards about to be drawn.
-- 3. __Never break the game.__ It only reads, never pushes, and never assumes something
--    named in a message is still in play.
module Marvel.Log.Narrator (narrationFor) where

import Marvel.Prelude
import Data.Aeson (Result (Success), fromJSON)
import Marvel.Window.Types qualified as W

import Marvel.Ability.Types (Ability (..), AbilityType (..))
import Data.HashMap.Strict qualified as HashMap
import Marvel.Campaign (CampaignRecord (..), CampaignStepKind (..))
import Marvel.Campaign qualified as Campaign
import Marvel.Area (Area (..), DeckKey (InfinityStoneDeckKey), Pile (..))
import Marvel.Card
import Marvel.EncounterCard (allEncounterCards)
import Marvel.PlayerCard (allPlayerCards)
import Marvel.Damage
import Marvel.Deck
import Marvel.Entity
import Marvel.Game
import Marvel.Id
import Marvel.Identity
import Marvel.Keyword (Keyword (Overkill, Piercing, Ranged))
import Marvel.Log
import Marvel.Ally.Types qualified as Ally
import Marvel.Matchers (EnemyMatcher (EnemyWithId, StunnedEnemy, ConfusedEnemy))
import Marvel.Query (selectAny)
import Marvel.Message
import Marvel.Minion.Types qualified as Minion
import Marvel.Modifier
import Marvel.Name
import Marvel.SideScheme.Types qualified as SideScheme
import Marvel.Support.Types qualified as Support
import Marvel.Phase
import Marvel.Question (Choice (ChangeForm))
import Marvel.Ref
import Marvel.Villain.Cards (allVillainsMap)
import Marvel.Scenario.Types qualified as Scenario
import Marvel.Villain.Types qualified as Villain

-- | What a message adds to the log, if anything. The match is pure, so a message that
-- says nothing costs nothing; only a narrating one reads the game.
narrationFor :: (HasGame m) => Message -> Maybe (m [LogEntry])
narrationFor = \case
  PrepareAllyDepartureByDefeat _ _ -> Nothing
  PrepareEnemyDamageGroup _ _ -> Nothing
  TakeEnemyDamageGroup _ _ _ -> Nothing
  ResolveEnemyDamageGroup messages cursor _ -> case messages !! cursor of
    VillainMessage vid inner -> villainNarration vid inner
    MinionMessage mid inner -> minionNarration mid inner
    message -> narrationFor message
  PrepareEnemyDamage _ -> Nothing
  ResolveEnemyDamage (VillainMessage vid inner) -> villainNarration vid inner
  ResolveEnemyDamage (MinionMessage mid inner) -> minionNarration mid inner
  ResolveEnemyDamage _ -> Nothing
  VillainMessage _ (VillainDamaged _ _) -> Nothing
  MinionMessage _ (MinionDamaged _ _) -> Nothing
  RemoveFromPlayAfterWindow {} -> Nothing
  AllocatedPayment {} -> Nothing
  SpentWithResources {} -> Nothing
  -- * Structure: the headings a reader finds their way by
  StartScenario -> Just do
    villains <- getsGame (Scenario.scenarioVillains . toAttrs . gameScenario)
    pure
      [ structure [ikeyPart "log.scenarioBegins" ["villain" ~> (codeRef RefVillain v) {logRefName = maybe (unCardCode v) (display . cdName) (lookup v allVillainsMap)}]]
      | v <- take 1 villains
      ]
  BeginPhase PlayerPhase -> Just do
    n <- getsGame gameRound
    pure [structure [ikeyPart "log.round" ["count" ~> n]], structure [ikeyPart "log.phase.player" []]]
  BeginPhase VillainPhase -> Just $ one $ structure [ikeyPart "log.phase.villain" []]
  -- the steps of the round (RR, Round Sequence), each a heading within its phase
  EndPhase PlayerPhase -> Just $ one $ structure [ikeyPart "log.step.endPlayerPhase" []]
  AccelerateMainScheme -> Just $ one $ structure [ikeyPart "log.step.placeThreat" []]
  VillainPhaseStep step -> Just $ one $ structure [ikeyPart (villainPhaseStepKey step) []]
  PassFirstPlayer -> Just do
    order <- getsGame gamePlayerOrder
    case drop 1 order <> take 1 order of
      next : _ -> do
        who <- playerRef' next
        one $ structure [ikeyPart "log.step.passFirstPlayer" ["player" ~> who]]
      [] -> pure []
  EndRound -> Just $ one $ structure [ikeyPart "log.step.endRound" []]
  -- a campaign game is won once its Victory steps are written in the log: told then
  GameOver Won -> Just do
    pending <- getsGame \g -> case gameCampaign g of
      Just c -> Campaign.currentScenario c == Just (toId $ gameScenario g)
      Nothing -> False
    pure [toned Good $ structure [ikeyPart "log.gameOver.won" []] | not pending]
  -- a heading for a part of the Campaign Instructions only when it has something to do
  CampaignStep step -> Just do
    told <- getsGame \g -> case gameCampaign g of
      Nothing -> False
      Just c -> case step of
        CampaignChoices -> any (notNull . (.pending)) c.campaignLog.players
        CampaignSetup -> Campaign.scenarioNumber c > 1 || c.kind /= Campaign.RiseOfRedSkull
        CampaignSetupThen _ -> False
        CampaignVictory -> True
    pure [structure [ikeyPart (campaignStepKey step) []] | told]
  RecordInCampaignLog r -> campaignNarration r
  GameOver Lost -> Just $ one $ toned Bad $ structure [ikeyPart "log.gameOver.lost" []]
  -- * What the players do
  IdentityMessage ident msg -> identityNarration ident msg
  PutTopEncounterCardInPlayerDeck ident -> Just do
    who <- playerRef' ident
    one $ notice [ikeyPart "log.encounterCardShuffledIntoPlayerDeck" ["player" ~> who]]
  -- changing form is told as the change itself
  UsedAbility _ a | ChangeForm `elem` a.abilityChoices -> Nothing
  UsedAbility ident a | narrates a.abilityType -> Just $ whenRef (refocus a.abilitySource) \source -> do
    who <- playerRef' ident
    one $ action [ikeyPart "log.usesAbility" ["player" ~> who, "card" ~> source]]
  -- Departure wrappers are timing plumbing; the continued message is narrated.
  ResolveMinionLeavingPlay mid (MinionMessage _ MinionLeavesPlay) -> minionNarration mid MinionLeavesPlay
  ResolveMinionLeavingPlay mid (CardSpecific _ key _)
    | key `elem` ["shuffleAfterLeaveWindow", "removeDefeatedSleeper"] -> minionNarration mid MinionLeavesPlay
  ResolveMinionLeavingPlay {} -> Nothing
  ResolveThreatRemoval {} -> Nothing
  PrepareDividedThreat {} -> Nothing
  Simultaneously msgs -> case mapMaybe narrationFor msgs of
    [] -> Nothing
    told -> Just $ concat <$> sequence told
  -- Choosing a contributor is payment plumbing; spending their cards/abilities is
  -- narrated by the same messages as any other payment.
  EventMessage _ (EventPaidWith _) -> Nothing
  EventMessage _ EventThwarted -> Nothing
  PlayCardWithReduction {} -> Nothing
  PlayCardIgnoringResourceCost {} -> Nothing
  ContributeResources {} -> Nothing
  DeclareContributionWilds {} -> Nothing
  ContributeCost {} -> Nothing
  PrepareAllianceCosts {} -> Nothing
  OfferAllianceCost {} -> Nothing
  ReserveAllyHeldCard {} -> Nothing
  AttachPlayerCardToMinion mid _ -> Just $ whenRef (MinionRef mid) \minion ->
    one $ mechanic [ikeyPart "log.attachesPlayerCardToMinion" ["minion" ~> minion]]
  AttachTreacheryToAlly aid _ -> Just $ whenRef (AllyRef aid) \ally ->
    one $ mechanic [ikeyPart "log.attachesTreacheryToAlly" ["ally" ~> ally]]
  DivideAttackDamage {} -> Nothing
  AssignEvenDamage {} -> Nothing
  IndirectDamageAndExhaust ident _ (damageAmount -> n) [] | n > 0 -> Just do
    who <- playerRef' ident
    one $ toned Bad $ mechanic [ikeyPart "log.takesIndirectDamage" ["player" ~> who, "count" ~> n]]
  ExhaustIfDamaged {} -> Nothing
  AttachPlayableEventToAlly aid _ -> Just $ whenRef (AllyRef aid) \ally ->
    one $ mechanic [ikeyPart "log.attachesFacedownEvent" ["ally" ~> ally]]
  SwapWeatherSupport you _ card _ -> Just do
    who <- playerRef' you
    one $ action [ikeyPart "log.changesWeather" ["player" ~> who, "card" ~> PlayerCard card]]
  ResolveCurrentWeatherSpecial {} -> Nothing
  ResolveWeatherSpecial you sid -> Just do
    resolves <- asYou you $ weatherSpecialCanResolve you sid
    if resolves then whenRef (SupportRef sid) (\support -> do
      who <- playerRef' you
      one $ action [ikeyPart "log.resolvesWeatherSpecial" ["player" ~> who, "card" ~> support]])
    else pure []
  WeatherSpecialFinished {} -> Nothing
  -- * Enemies
  VillainMessage vid msg -> villainNarration vid msg
  MinionMessage mid msg -> minionNarration mid msg
  AllyMessage aid msg -> allyNarration aid msg
  -- an energy form turned faceup is the form its player changes to (Spectrum)
  UpgradeMessage _ (UpgradeHostBecameMinion _) -> Nothing
  UpgradeMessage uid FlipUpgradeFaceUp -> Just $ whenRef (UpgradeRef uid) \form ->
    getSourceController (UpgradeRef uid) >>= \case
      Just p -> do
        who <- playerRef' p
        one $ action [ikeyPart "log.changesToEnergyForm" ["player" ~> who, "form" ~> form]]
      Nothing -> pure []
  CardSpecific (UpgradeRef uid) "flipPsiEnergy" _ -> Just do
    locked <- elem CannotFlipPsiEnergy <$> getModifiers (UpgradeRef uid)
    if locked then pure [] else simply "log.flips" (UpgradeRef uid)
  CardSpecific (UpgradeRef uid) "flipMassForm" _ -> Just $ whenRef (UpgradeRef uid) \form ->
    getSourceController (UpgradeRef uid) >>= \case
      Just p -> do
        who <- playerRef' p
        one $ action [ikeyPart "log.changesMassForm" ["player" ~> who]]
      Nothing -> pure []
  -- * Schemes
  MainSchemeMessage sid msg -> schemeNarration (MainSchemeRef sid) msg
  SideSchemeMessage sid msg -> sideSchemeNarration sid msg
  NextMainScheme _ -> Just $ one $ toned Bad $ mechanic [ikeyPart "log.mainSchemeAdvances" []]
  -- the active counter moves: told only when it goes to another villain
  SetActiveVillain vid -> Just do
    aid <- getAreaOfTarget vid
    current <- getActiveVillainIn aid
    if current == Just vid then pure [] else whenRef (VillainRef vid) \v -> one $ mechanic [ikeyPart "log.activeVillain" ["villain" ~> v]]
  -- * The encounter deck
  -- Name-before-looking interrupts resolve before revealing the card to the table.
  CardSpecific GameRef key (fromJSON -> Success (_ :: W.RevealSource, ident :: IdentityId, card :: EncounterCard)) | key `elem` ["revealAfterWould", "revealAfterWouldWithoutWhenRevealed"] -> Just do
    who <- playerRef' ident
    one $ mechanic [ikeyPart "log.reveals" ["player" ~> who, "card" ~> EncounterCard card]]
  GiveStatus target s
    | s == ToughStatus -> Just $ simply (statusKey s) target
    | otherwise -> Just $ unlessStalwart target $ simply (statusKey s) target
  FriendlyCharacterAttacks attacker target -> Just $ whenRef (characterRef attacker) \who -> do
    victim <- playerRef' target
    one $ action [ikeyPart "log.friendlyCharacterAttacks" ["attacker" ~> who, "player" ~> victim]]
  RevealBoostCard card _ -> Just $ one $ notice [ikeyPart "log.boost" ["card" ~> EncounterCard card]]
  RevealBoostCardWithoutAbility card _ -> Just $ one $ notice [ikeyPart "log.boost" ["card" ~> EncounterCard card]]
  -- a player's event into the encounter deck (a Cosmic Entity, Adam Warlock): played
  -- openly, so named
  ShuffleIntoEncounterDeck cards
    | events@(_ : _) <- [c | c <- cards, cdCardType (getCardDef c) == EventType] ->
        Just $ concatMapM (\c -> one $ mechanic [ikeyPart "log.shuffledIntoEncounterDeck" ["card" ~> EncounterCard c]]) events
  -- an Infinity Stone put into play from its deck (The Mad Titan's Shadow)
  UsingDeck InfinityStoneDeckKey (PutTopOfEncounterDeckIntoPlay _) -> Just do
    top <- take 1 . pileDeck <$> getPile InfinityStoneDeckKey
    concatMapM (\c -> one $ toned Bad $ mechanic [ikeyPart "log.infinityStoneIntoPlay" ["card" ~> EncounterCard c]]) top
  TakeControlOfMinion you mid -> Just $ whenRef (MinionRef mid) \card -> do
    who <- playerRef' you
    one $ mechanic [ikeyPart "log.controlledMinion" ["player" ~> who, "card" ~> card]]
  RestoreControlledMinion aid -> Just $ simply "log.restoredMinion" (AllyRef aid)
  UpgradeMessage uid (AddUpgradeUses n) | n > 0 -> Just $ counted "log.countersPlaced" (UpgradeRef uid) n
  VillainMessage vid (VillainSchemeRemovesThreat _ _) -> Just $ simply "log.schemeRemovesThreat" (VillainRef vid)
  VillainMessage vid (VillainAttackDamageRedirected enemy) -> Just $ redirected (VillainRef vid) (enemyRef enemy)
  MinionMessage mid (MinionAttackDamageRedirected enemy) -> Just $ redirected (MinionRef mid) (enemyRef enemy)
  PlaceCharacterThreat character n | n > 0 -> Just $ counted "log.characterThreatPlaced" (characterRef character) n
  RemoveCharacterThreat character n | n > 0 -> Just $ counted "log.characterThreatRemoved" (characterRef character) n
  MoveCharacterThreatToMainScheme character -> Just $ do
    n <- getCharacterThreat character
    if n == 0 then pure [] else counted "log.characterThreatMoved" (characterRef character) n
  PlaceCounters target n | n > 0 -> Just $ counted "log.countersPlaced" target n
  RemoveCounters target n | n > 0 -> Just $ counted "log.countersRemoved" target n
  EnvironmentMessage eid FlipEnvironment -> Just $ simply "log.flips" (EnvironmentRef eid)
  KeptOutByUnique t -> Just $ simply "log.keptOutByUnique" t
  -- The Galaxy's Most Wanted
  AddToVictoryDisplay t -> Just $ simply "log.victoryDisplay" t
  -- Loki
  AddCardToVictoryDisplay card -> Just $ one $ mechanic [ikeyPart "log.victoryDisplay" ["target" ~> EncounterCard card]]
  ReplaceVillain vid _ Swapped -> Just $ whenRef (VillainRef vid) \v -> one $ toned Bad $ mechanic [ikeyPart "log.villainSwapped" ["villain" ~> v]]
  SetPeril (Just ident) -> Just do
    who <- playerRef' ident
    one $ notice [ikeyPart "log.peril" ["player" ~> who]]
  -- Infiltrate the Museum: told as it arrives, not as a card in play sets out for it
  PlaceInCollection card -> Just do
    g <- getGame
    if isJust (inPlayRef g card) then pure [] else one $ toned Bad $ mechanic [ikeyPart "log.collection" ["card" ~> card]]
  AttachmentMessage aid (AttachedToIdentity ident) -> Just $ attachedTo aid (IdentityRef ident)
  AttachmentMessage aid (AttachedToSideScheme sid) -> Just $ attachedTo aid (SideSchemeRef sid)
  -- Tower Defense
  AttachmentMessage aid (AttachedToMainScheme mid) -> Just $ attachedTo aid (MainSchemeRef mid)
  EnvironmentMessage eid (EnvironmentDamaged n) | n > 0 -> Just $ counted "log.takesDamage" (EnvironmentRef eid) n
  -- Ebony Maw
  AttachmentMessage aid (AttachedToAlly ally) -> Just $ attachedTo aid (AllyRef ally)
  EnvironmentMessage eid (EnvironmentOwnedBy ident) -> Just $ whenRef (EnvironmentRef eid) \e -> do
    who <- playerRef' ident
    one $ toned Bad $ mechanic [ikeyPart "log.inPlayAreaOf" ["card" ~> e, "player" ~> who]]
  DiscardUntilIntoPlay ident _ -> Just do
    who <- playerRef' ident
    one $ mechanic [ikeyPart "log.discardsUntilIntoPlay" ["player" ~> who]]
  -- put into play attached, not revealed: the card is not in play yet to be named
  AttachEncounterCard card enemy -> Just $ whenRef (EnemyRef enemy) \t ->
    one $ toned Bad $ mechanic [ikeyPart "log.attachedTo" ["card" ~> EncounterCard card, "target" ~> t]]
  -- * Game areas
  CreateArea _ ps -> Just do
    who <- traverse playerRef' ps
    pure [mechanic [ikeyPart "log.areaCreated" ["player" ~> p]] | p <- who]
  -- the players of an area join another's; an area with none left says nothing
  CombineAreas from into -> Just do
    areas <- getAreas
    let playersOf aid = concat [areaPlayers a | a <- areas, areaId a == aid]
    who <- traverse playerRef' (playersOf from)
    others <- traverse playerRef' (take 1 $ playersOf into)
    pure case others of
      other : _ -> [mechanic [ikeyPart "log.areaJoins" ["player" ~> p, "other" ~> other]] | p <- who]
      [] -> [mechanic [ikeyPart "log.areaJoinsCentral" ["player" ~> p]] | p <- who]
  RemovePlayerObligationsFromGame -> Just $ one $ mechanic [ikeyPart "log.playerObligationsRemoved" []]
  DiscardTopOfEncounterDeck n _ | n > 0 -> Just $ one $ mechanic [ikeyPart "log.encounterCardsDiscarded" ["count" ~> n]]
  -- each card's count as the window to counting it left it
  WithBoostIcons _ _ counts | notNull counts ->
    Just $ pure [mechanic [ikeyPart "log.boostIconsCounted" ["card" ~> EncounterCard c, "count" ~> n]] | (c, n) <- counts]
  -- told once, as it is dealt; each character's share is told as it takes it
  AssignIndirectDamage ident _ (damageAmount -> n) [] | n > 0 -> Just do
    who <- playerRef' ident
    one $ toned Bad $ mechanic [ikeyPart "log.takesIndirectDamage" ["player" ~> who, "count" ~> n]]
  AssignIndirectDamageAmongPlayers _ n [] | n > 0 -> Just $ one $ toned Bad $ mechanic [ikeyPart "log.indirectDamageAmongPlayers" ["count" ~> n]]
  -- Hela, Enchantress: an ally treated as a minion, and back
  PutAllyIntoPlayAsMinion ident card -> Just do
    who <- playerRef' ident
    one $ toned Bad $ mechanic [ikeyPart "log.allyIntoPlayAsMinion" ["card" ~> PlayerCard card, "player" ~> who]]
  AllyBecomesMinion aid -> Just $ whenRef (AllyRef aid) \a -> one $ toned Bad $ mechanic [ikeyPart "log.allyBecomesMinion" ["ally" ~> a]]
  MinionBecomesAlly mid -> Just $ whenRef (MinionRef mid) \m -> one $ mechanic [ikeyPart "log.minionBecomesAlly" ["ally" ~> m]]
  _ -> Nothing
 where
  statusKey = \case
    StunnedStatus -> "log.stunned"
    ConfusedStatus -> "log.confused"
    ToughStatus -> "log.tough"
  -- basic powers are told by what they do; resources by the card they pay for
  narrates = \case
    Basic -> False
    Resource -> False
    HeroResource -> False
    AlterEgoResource -> False
    Setup -> False
    _ -> True

campaignStepKey :: CampaignStepKind -> Text
campaignStepKey = \case
  CampaignChoices -> "log.campaign.choices"
  CampaignSetup -> "log.campaign.setup"
  CampaignVictory -> "log.campaign.victory"
  CampaignSetupThen _ -> "log.campaign.setup"

-- | What is written in the campaign log, told as it is.
campaignNarration :: (HasGame m) => CampaignRecord -> Maybe (m [LogEntry])
campaignNarration = \case
  RecordTechUpgrade who code -> Just $ added who code
  RecordConditionUpgrade who code -> Just $ added who code
  RecordObligation who code -> Just $ added who code
  RecordRescuedAllies who codes -> Just do
    p <- identityRef who
    one $ notice [ikeyPart "log.campaign.added" ["player" ~> p, "card" ~> map card codes]]
  RemoveFromCampaignLog _ code -> Just $ one $ notice [ikeyPart "log.campaign.crossedOut" ["card" ~> card code]]
  RecordExperimentalWeapons codes | notNull codes -> Just $ one $ notice [ikeyPart "log.campaign.recorded" ["what" ~> map card codes]]
  RecordDelayCounters n -> Just $ one $ notice [ikeyPart "log.campaign.delayCounters" ["count" ~> n]]
  RecordEngagedWithMinions who -> Just do
    p <- identityRef who
    one $ notice [ikeyPart "log.campaign.engaged" ["player" ~> p]]
  RecordAlliesRemoved codes | notNull codes -> Just $ one $ toned Bad $ notice [ikeyPart "log.campaign.removed" ["card" ~> map card codes]]
  RecordHitPoints who n -> Just do
    p <- identityRef who
    one $ notice [ikeyPart "log.campaign.hitPoints" ["player" ~> p, "count" ~> n]]
  RecordScenarioWon -> Just $ one $ toned Good $ structure [ikeyPart "log.campaign.scenarioWon" []]
  -- The Galaxy's Most Wanted
  RecordUnits who n -> Just do
    p <- identityRef who
    one $ notice [ikeyPart "log.campaign.units" ["player" ~> p, "count" ~> n]]
  SpendUnits who n -> Just do
    p <- identityRef who
    one $ notice [ikeyPart "log.campaign.unitsSpent" ["player" ~> p, "count" ~> n]]
  RecordCollection who codes -> Just do
    p <- identityRef who
    one $ notice [ikeyPart "log.campaign.collection" ["player" ~> p, "card" ~> map card codes]]
  RecordGalacticArtifacts codes | notNull codes -> Just $ one $ notice [ikeyPart "log.campaign.recorded" ["what" ~> map card codes]]
  RecordPowerStoneControl who -> Just do
    p <- identityRef who
    one $ notice [ikeyPart "log.campaign.powerStone" ["player" ~> p]]
  RecordHeadhunterDefeated _ -> Just $ one $ notice [ikeyPart "log.campaign.headhunterDefeated" []]
  RecordEvasionCounters n -> Just $ one $ notice [ikeyPart "log.campaign.evasionCounters" ["count" ~> n]]
  -- The Mad Titan's Shadow
  RecordTitanMark m -> Just $ one $ notice [ikeyPart ("log.campaign.titanMark." <> tshow m) []]
  RecordReputation n -> Just $ one $ notice [ikeyPart "log.campaign.reputation" ["count" ~> n]]
  RecordShieldTech who code -> Just $ added who code
  RecordAspectAdvantage who code _ -> Just $ added who code
  RecordPlanningAhead who code -> Just do
    p <- identityRef who
    one $ notice [ikeyPart "log.campaign.planningAhead" ["player" ~> p, "card" ~> card code]]
  RecordCommunityService codes -> Just $ one $ notice [ikeyPart "log.campaign.recorded" ["what" ~> map card codes]]
  RecordOsbornTech code -> Just $ one $ notice [ikeyPart "log.campaign.recorded" ["what" ~> [card code]]]
  RecordWakingNightmare n -> Just $ one $ notice [ikeyPart "log.campaign.wakingNightmare" ["count" ~> n]]
  RecordLastOnesStanding codes -> Just $ one $ notice [ikeyPart "log.campaign.recorded" ["what" ~> map card codes]]
  RecordNextEvolutionScheme _ code -> Just $ one $ notice [ikeyPart "log.campaign.recorded" ["what" ~> [card code]]]
  RecordNextEvolutionEnvironment code -> Just $ one $ notice [ikeyPart "log.campaign.recorded" ["what" ~> [card code]]]
  RecordNextEvolutionMarauders codes -> Just $ one $ notice [ikeyPart "log.campaign.recorded" ["what" ~> map card codes]]
  RecordNextEvolutionMorlocks n -> Just $ one $ notice [ikeyPart "log.campaign.morlocksSaved" ["count" ~> n]]
  RecordNextEvolutionHopeDamage scenario n -> Just $ one $ notice [ikeyPart "log.campaign.hopeDamage" ["count" ~> n, "scenario" ~> scenario]]
  RecordFinalReputation -> Just $ one $ notice [ikeyPart "log.campaign.finalReputation" []]
  _ -> Nothing
 where
  added who code = do
    p <- identityRef who
    one $ notice [ikeyPart "log.campaign.added" ["player" ~> p, "card" ~> card code]]
  card code = (codeRef RefCard code) {logRefName = maybe (unCardCode code) (display . cdName) (lookup code allPlayerCards <|> lookup code allEncounterCards)}
  -- the player the log knows by their identity, if they are still at the table
  identityRef code = do
    players <- getsGame (HashMap.elems . gamePlayers)
    case [p | p <- players, toHeroCardCode (toCardCode p) == code] of
      p : _ -> playerRef' (toId p)
      [] -> pure $ codeRef RefIdentity code

identityNarration :: (HasGame m) => IdentityId -> IdentityMessage -> Maybe (m [LogEntry])
identityNarration ident = \case
  SwapIdentityVersion heroCode _ -> Just do
    who <- playerRef' ident
    one $ mechanic [ikeyPart "log.identityVersionSwapped" ["player" ~> who, "version" ~> (case heroCode of "29002a" -> (2 :: Natural); "29003a" -> 3; _ -> 1)]]
  IdentityDealtBoost _ -> Just do
    who <- playerRef' ident
    one $ notice [ikeyPart "log.identityDealtBoost" ["player" ~> who]]
  MoveIdentityBoostCardsToVillain _ -> Just do
    who <- playerRef' ident
    one $ notice [ikeyPart "log.identityBoostsMoved" ["player" ~> who]]
  UpgradeControlLost _ -> Nothing
  -- a card that was nobody's, as a rescued Captive ally, becomes the player's
  AddToHand card | isJust (pcOwner card) -> Just do
    unowned <- getsGame \g -> any (any (\c -> pcCardId c == pcCardId card && isNothing (pcOwner c)) . SideScheme.sideSchemeHeldCards . toAttrs) (gameSideSchemes g)
    if unowned
      then do
        who <- playerRef' ident
        one $ toned Good $ action [ikeyPart "log.takesControlOf" ["player" ~> who, "card" ~> PlayerCard card]]
      else pure []
  BeginTurn -> Just do
    who <- playerRef' ident
    one $ structure [ikeyPart "log.turn" ["player" ~> who]]
  PlayedCard card _ -> Just do
    who <- playerRef' ident
    one $ action [ikeyPart "log.plays" ["player" ~> who, "card" ~> PlayerCard card]]
  ChangedToForm side -> Just do
    who <- playerRef' ident
    face <- getsGame \g -> do
      p <- lookup ident (gamePlayers g)
      faceDef <$> lookup side (toAttrs p).playerIdentitySides
    pure
      [ action [ikeyPart "log.changesForm" ["player" ~> who, "identity" ~> (codeRef RefIdentity (cdCardCode def)) {logRefName = display (cdName def)}]]
      | Just def <- [face]
      ]
  SideMessage Attacked -> Just $ simplyPlayer "log.basicAttack"
  SideMessage Thwarted -> Just $ simplyPlayer "log.basicThwart"
  SideMessage Recovered -> Just $ simplyPlayer "log.recovers"
  SideMessage (RecoveredWithBonus _) -> Just $ simplyPlayer "log.recovers"
  SideMessage (DefendedWithoutExhausting _ enemy) -> identityNarration ident (SideMessage $ Defended enemy)
  SideMessage (DefenseUsingAttackApplied {}) -> Nothing
  SideMessage (Defended enemy) -> Just $ whenRef (EnemyRef enemy) \e -> do
    who <- playerRef' ident
    one $ action [ikeyPart "log.defends" ["player" ~> who, "enemy" ~> e]]
  DrawCards FromDeck n | n > 0 -> Just do
    who <- playerRef' ident
    drawn <- getsGame $ maybe [] (filter (isNothing . playerZoneEncounterCard) . take (fromIntegral n) . unDeck . (.playerIdentityDeck) . toAttrs) . lookup ident . gamePlayers
    pure
      [ forPlayer ident $ notice [ikeyPart "log.drawsCards" ["player" ~> who, "cards" ~> map PlayerCard drawn]]
      , exceptPlayer ident $ notice [ikeyPart "log.draws" ["player" ~> who, "count" ~> length drawn]]
      ]
  IdentityDamaged _ dmg | damageAmount dmg > 0 -> Just do
    tough <- getsGame $ maybe False (playerIdentityTough . toAttrs) . lookup ident . gamePlayers
    immune <- elem CannotTakeDamage <$> getModifiers (IdentityRef ident)
    if immune || (tough && preventedByTough dmg)
      then damaged (IdentityRef ident) (tough && preventedByTough dmg) (damageAmount dmg)
      else do
        who <- playerRef' ident
        one $ toned Bad $ mechanic [ikeyPart "log.takesDamage" ["target" ~> who, "count" ~> damageAmount dmg]]
  IdentityHealed n | n > 0 -> Just do
    who <- playerRef' ident
    damage <- getsGame $ maybe 0 identityDamage . lookup ident . gamePlayers
    let healed = min n damage
    pure [toned Good $ mechanic [ikeyPart "log.heals" ["target" ~> who, "count" ~> healed]] | healed > 0]
  IdentitySetHitPoints n -> Just do
    who <- playerRef' ident
    one $ toned Good $ mechanic [ikeyPart "log.hitPointsSetTo" ["target" ~> who, "count" ~> n]]
  IdentityDefeated -> Just do
    who <- playerRef' ident
    one $ toned Bad $ mechanic [ikeyPart "log.eliminated" ["player" ~> who]]
  IdentityStunned -> Just $ status "log.stunned"
  IdentityConfused -> Just $ status "log.confused"
  IdentityBecomeTough -> Just $ status "log.tough"
  _ -> Nothing
 where
  simplyPlayer key = do
    who <- playerRef' ident
    one $ action [ikeyPart key ["player" ~> who]]
  status key = do
    who <- playerRef' ident
    one $ toned Bad $ mechanic [ikeyPart key ["target" ~> who]]

villainNarration :: (HasGame m) => VillainId -> VillainMessage -> Maybe (m [LogEntry])
villainNarration vid = \case
  VillainDefeatPending _ -> Nothing
  VillainResolveDefeat _ _ -> Nothing
  -- told as each gets under way, so one an ability replaces (Norman Osborn's attack, Green
  -- Goblin's scheme) is told as what it is replaced with; a stun or confusion as it is spent
  VillainAttacks ident -> Just $ whenM' (selectAny (EnemyWithId (EnemyVillainId vid) <> StunnedEnemy)) $ activation (VillainRef vid) ident "log.attacks" True
  VillainAttacksWithoutBoost ident -> villainNarration vid (VillainAttacks ident)
  VillainAttacksGaining ident _ -> villainNarration vid (VillainAttacks ident)
  VillainBeginAttack ident -> Just $ activation (VillainRef vid) ident "log.attacks" False
  VillainBeginAttackWithoutBoost ident -> Just $ activation (VillainRef vid) ident "log.attacks" False
  VillainSchemes -> Just $ whenM' (selectAny (EnemyWithId (EnemyVillainId vid) <> ConfusedEnemy)) $ schemes (VillainRef vid) True
  VillainBeginScheme -> Just $ schemes (VillainRef vid) False
  VillainDamaged source dmg | damageAmount dmg > 0 -> Just do
    tough <- getsGame $ maybe False Villain.villainIsTough . lookup vid . gameVillains
    -- none from a player card (Sanctuary)
    shielded <- cannotTakeDamageFrom (VillainRef vid) source
    if shielded
      then whenRef (VillainRef vid) \t -> one $ toned Good $ mechanic [ikeyPart "log.takesNoDamage" ["target" ~> t]]
      else do
        -- what gets past a tough status card is reduced (Thanos's Armor)
        incoming <- incomingEnemyDamage (EnemyVillainId vid) source dmg
        damaged (VillainRef vid) (tough && preventedByTough dmg) (damageAmount incoming)
  VillainHealed n | n > 0 -> Just $ healedBy (VillainRef vid) n
  VillainDefeated -> Just $ whenRef (VillainRef vid) \v -> one $ toned Good $ mechanic [ikeyPart "log.defeated" ["target" ~> v]]
  VillainAdvanced -> Just $ whenRef (VillainRef vid) \v -> one $ toned Bad $ mechanic [ikeyPart "log.villainAdvances" ["villain" ~> v]]
  FlipVillain -> Just $ simply "log.flips" (VillainRef vid)
  -- a stalwart villain is not stunned or confused: nothing to tell
  VillainStunned _ -> Just $ unlessStalwart (VillainRef vid) $ simply "log.stunned" (VillainRef vid)
  VillainConfused _ -> Just $ unlessStalwart (VillainRef vid) $ simply "log.confused" (VillainRef vid)
  VillainBecomeTough -> Just $ simply "log.tough" (VillainRef vid)
  VillainAttackGains k -> gains (VillainRef vid) k
  VillainAttackChanged n | n /= 0 -> Just $ attackChanged (VillainRef vid) n
  -- told as they are added, so icons cancelled as the card turns up are not
  VillainAddBoost n | n > 0 -> Just $ whenRef (VillainRef vid) \v -> one $ notice [ikeyPart "log.boostAdded" ["enemy" ~> v, "count" ~> n]]
  _ -> Nothing

whenM' :: (Monad m) => m Bool -> m [LogEntry] -> m [LogEntry]
whenM' cond body = cond >>= \b -> if b then body else pure []

minionNarration :: (HasGame m) => MinionId -> MinionMessage -> Maybe (m [LogEntry])
minionNarration mid = \case
  MinionEngagedIdentity ident -> Just $ whenRef (MinionRef mid) \m -> do
    who <- playerRef' ident
    one $ mechanic [ikeyPart "log.engages" ["minion" ~> m, "player" ~> who]]
  MinionAttacksWithChange ident _ _ -> Just $ activation (MinionRef mid) ident "log.attacks" =<< selectAny (EnemyWithId (EnemyMinionId mid) <> StunnedEnemy)
  MinionAttacks ident -> Just $ activation (MinionRef mid) ident "log.attacks" =<< selectAny (EnemyWithId (EnemyMinionId mid) <> StunnedEnemy)
  MinionAttacksEnemy enemy -> Just $ whenRef (MinionRef mid) \m -> do
    stunned <- selectAny (EnemyWithId (EnemyMinionId mid) <> StunnedEnemy)
    whenRef (EnemyRef enemy) \e -> one $
      if stunned
        then toned Good $ mechanic [ikeyPart "log.stunnedInstead" ["enemy" ~> m]]
        else mechanic [ikeyPart "log.attacks" ["enemy" ~> m, "player" ~> e]]
  MinionAttackEnemyAfterInterrupt {} -> Nothing
  MinionSchemesAgainst {} -> Nothing
  MinionSchemeAgainstEnded -> Nothing
  MinionSchemes -> Just $ schemes (MinionRef mid) =<< selectAny (EnemyWithId (EnemyMinionId mid) <> ConfusedEnemy)
  MinionDamaged source dmg | damageAmount dmg > 0 -> Just do
    tough <- getsGame $ maybe False ((.minionTough) . toAttrs) . lookup mid . gameMinions
    incoming <- incomingEnemyDamage (EnemyMinionId mid) source dmg
    damaged (MinionRef mid) (tough && preventedByTough dmg) (damageAmount incoming)
  MinionHealed n | n > 0 -> Just $ healedBy (MinionRef mid) n
  MinionDefeated -> Nothing
  MinionLeavesPlay -> Just $ whenRef (MinionRef mid) \m -> one $ toned Good $ mechanic [ikeyPart "log.defeated" ["target" ~> m]]
  MinionStunned _ -> Just $ unlessStalwart (MinionRef mid) $ simply "log.stunned" (MinionRef mid)
  MinionConfused _ -> Just $ unlessStalwart (MinionRef mid) $ simply "log.confused" (MinionRef mid)
  MinionBecomeTough -> Just $ simply "log.tough" (MinionRef mid)
  MinionAttackGains k -> gains (MinionRef mid) k
  MinionAttackChanged n | n /= 0 -> Just $ attackChanged (MinionRef mid) n
  MinionAddBoost n | n > 0 -> Just $ whenRef (MinionRef mid) \m -> one $ notice [ikeyPart "log.boostAdded" ["enemy" ~> m, "count" ~> n]]
  _ -> Nothing

-- | "The attack gains piercing": told for the keywords an attack can gain.
gains :: (HasGame m) => Target -> Keyword -> Maybe (m [LogEntry])
gains enemy = \case
  Overkill -> Just $ simply "log.attackGainsOverkill" enemy
  Piercing -> Just $ simply "log.attackGainsPiercing" enemy
  Ranged -> Just $ simply "log.attackGainsRanged" enemy
  _ -> Nothing

-- | "That enemy gets -3 ATK for that attack."
attackChanged :: (HasGame m) => Target -> Int -> m [LogEntry]
attackChanged enemy n = whenRef enemy \e ->
  one $ notice [ikeyPart (if n > 0 then "log.attackRaised" else "log.attackLowered") ["enemy" ~> e, "count" ~> abs n]]

allyNarration :: (HasGame m) => AllyId -> AllyMessage -> Maybe (m [LogEntry])
allyNarration aid = \case
  AllyAttackConsequential -> Nothing
  AllyAttackConsequentialAgainst _ -> Nothing
  AllyAssignEvenAttack {} -> Nothing
  AllyEvenAttackLands {} -> Nothing
  AllyEvenAttackFinished {} -> Nothing
  AllyAttackMultiple {} -> Nothing
  AllyAttackMultipleNext {} -> Nothing
  AllyAttackMultipleRetaliation {} -> Nothing
  AllyAttackMultipleFinished {} -> Nothing
  AllyAttacked -> Just $ whenRef (AllyRef aid) \a -> one $ action [ikeyPart "log.allyAttacks" ["ally" ~> a]]
  AllyThwarted -> Just $ whenRef (AllyRef aid) \a -> one $ action [ikeyPart "log.allyThwarts" ["ally" ~> a]]
  AllyDefended enemy -> Just $ whenRef (AllyRef aid) \a -> whenRef (EnemyRef enemy) \e ->
    one $ action [ikeyPart "log.allyDefends" ["ally" ~> a, "enemy" ~> e]]
  AllyDamaged _ dmg | damageAmount dmg > 0 -> Just do
    tough <- getsGame $ maybe False ((.allyTough) . toAttrs) . lookup aid . gameAllies
    damaged (AllyRef aid) (tough && preventedByTough dmg) (damageAmount dmg)
  AllyHealed n | n > 0 -> Just $ healedBy (AllyRef aid) n
  AllyDefeatedByDamage -> allyNarration aid AllyDefeated
  AllyLeavePlayByDefeat -> Nothing
  AllyDefeatedByAttack _ -> allyNarration aid AllyDefeated
  AllyDefeated -> Just $ whenRef (AllyRef aid) \a -> one $ toned Bad $ mechanic [ikeyPart "log.defeated" ["target" ~> a]]
  AllyStunned -> Just $ simply "log.stunned" (AllyRef aid)
  FlipAlly -> Just $ simply "log.flips" (AllyRef aid)
  -- Hela: Odin following the first player
  AllyChangedController ident -> Just $ whenRef (AllyRef aid) \a -> do
    who <- playerRef' ident
    one $ mechanic [ikeyPart "log.allyChangedController" ["ally" ~> a, "player" ~> who]]
  _ -> Nothing

schemeNarration :: (HasGame m) => Target -> MainSchemeMessage -> Maybe (m [LogEntry])
schemeNarration target = \case
  SetMainSchemeGlider True -> Just $ simply "log.gliderMoved" target
  AddMainSchemeAcceleration n | n > 0 -> Just $ counted "log.accelerationPlaced" target n
  MainSchemePlaceThreat n | n > 0 -> Just $ threat target "log.threatPlaced" n Bad
  MainSchemeBasicThwarted _ n | n > 0 -> Just $ threat target "log.threatRemoved" n Good
  MainSchemeThwarted _ n | n > 0 -> Just $ threat target "log.threatRemoved" n Good
  MainSchemeThreatRemoved _ n | n > 0 -> Just $ threat target "log.threatRemoved" n Good
  _ -> Nothing

sideSchemeNarration :: (HasGame m) => SideSchemeId -> SideSchemeMessage -> Maybe (m [LogEntry])
sideSchemeNarration sid = \case
  ExhaustedSideScheme -> Just $ simply "log.exhausted" (SideSchemeRef sid)
  ReadiedSideScheme -> Just $ simply "log.readied" (SideSchemeRef sid)
  SideSchemePlaceAccelerationToken -> Just $ counted "log.accelerationPlaced" (SideSchemeRef sid) 1
  RevealSideScheme -> Just $ simply "log.sideSchemeRevealed" (SideSchemeRef sid)
  SideSchemePlaceThreat n | n > 0 -> Just $ threat (SideSchemeRef sid) "log.threatPlaced" n Bad
  SideSchemeThwarted _ n | n > 0 -> Just $ threat (SideSchemeRef sid) "log.threatRemoved" n Good
  SideSchemeThreatRemoved _ n | n > 0 -> Just $ threat (SideSchemeRef sid) "log.threatRemoved" n Good
  DefeatSideScheme -> Just $ whenRef (SideSchemeRef sid) \s -> one $ toned Good $ mechanic [ikeyPart "log.defeated" ["target" ~> s]]
  _ -> Nothing

-- * Pieces

one :: (Applicative m) => LogEntry -> m [LogEntry]
one = pure . pure

-- | What is told, unless the enemy is stalwart and the status did not take.
unlessStalwart :: (HasGame m) => Target -> m [LogEntry] -> m [LogEntry]
unlessStalwart target told = isStalwartEnemy target >>= \stalwart -> if stalwart then pure [] else told

simply :: (HasGame m) => Text -> Target -> m [LogEntry]
simply key target = whenRef target \t -> one $ mechanic [ikeyPart key ["target" ~> t]]

whenRef :: (HasGame m) => Target -> (LogRef -> m [LogEntry]) -> m [LogEntry]
whenRef target f = maybe (pure []) f =<< refFor target

-- | An enemy activating against a player, or shaking off the status that stops it.
activation :: (HasGame m) => Target -> IdentityId -> Text -> Bool -> m [LogEntry]
activation enemy ident key blocked = whenRef enemy \e -> do
  who <- playerRef' ident
  one $
    if blocked
      then toned Good $ mechanic [ikeyPart "log.stunnedInstead" ["enemy" ~> e]]
      else mechanic [ikeyPart key ["enemy" ~> e, "player" ~> who]]

schemes :: (HasGame m) => Target -> Bool -> m [LogEntry]
schemes enemy confused = whenRef enemy \e ->
  one $
    if confused
      then toned Good $ mechanic [ikeyPart "log.confusedInstead" ["enemy" ~> e]]
      else mechanic [ikeyPart "log.schemes" ["enemy" ~> e]]

-- | Damage, the tough status it takes off instead, or none at all for a character that
-- cannot take damage (Red Skull, while The Red House is in play).
damaged :: (HasGame m) => Target -> Bool -> Natural -> m [LogEntry]
damaged target tough n = whenRef target \t -> do
  prevented <- elem CannotTakeDamage <$> getModifiers target
  one $
    if
      | prevented -> toned Good $ mechanic [ikeyPart "log.takesNoDamage" ["target" ~> t]]
      | tough -> mechanic [ikeyPart "log.toughRemoved" ["target" ~> t]]
      | otherwise -> mechanic [ikeyPart "log.takesDamage" ["target" ~> t, "count" ~> n]]

attachedTo :: (HasGame m) => AttachmentId -> Target -> m [LogEntry]
attachedTo aid target = whenRef (AttachmentRef aid) \a -> whenRef target \t ->
  one $ toned Bad $ mechanic [ikeyPart "log.attachedTo" ["card" ~> a, "target" ~> t]]

counted :: (HasGame m) => Text -> Target -> Natural -> m [LogEntry]
counted key target n = whenRef target \t -> one $ mechanic [ikeyPart key ["target" ~> t, "count" ~> n]]

healedBy :: (HasGame m) => Target -> Natural -> m [LogEntry]
healedBy target n = whenRef target \t -> one $ mechanic [ikeyPart "log.heals" ["target" ~> t, "count" ~> n]]

threat :: (HasGame m) => Target -> Text -> Natural -> LogTone -> m [LogEntry]
threat target key n tone = whenRef target \t -> one $ toned tone $ mechanic [ikeyPart key ["scheme" ~> t, "count" ~> n]]

-- | A player, as their name and the face they show.
playerRef' :: (HasGame m) => IdentityId -> m LogRef
playerRef' ident =
  getsGame (lookup ident . gamePlayers) <&> \case
    Just p -> (playerRef ident (toAttrs p).playerIdentityName) {logRefCardCode = Just (toCardCode p)}
    Nothing -> playerRef ident ""

faceDef :: PlayerIdentitySide -> CardDef
faceDef = \case
  HeroSide x -> getCardDef x
  AlterEgoSide x -> getCardDef x

codeRef :: LogRefKind -> CardCode -> LogRef
codeRef kind code = (logRef kind (unCardCode code)) {logRefCardCode = Just code}

-- | Something in play, as a chip, if it is still there to be named.
refFor :: (HasGame m) => Target -> m (Maybe LogRef)
refFor = \case
  IdentityRef ident -> getsGame (lookup ident . gamePlayers) >>= traverse \p -> do
    r <- playerRef' ident
    pure r {logRefKind = RefIdentity, logRefName = display (cdName (getCardDef p))}
  VillainRef x -> byId RefVillain x gameVillains getCardDef
  MinionRef x -> byId RefMinion x gameMinions getCardDef
  AllyRef x -> byId RefAlly x gameAllies getCardDef
  SupportRef x -> byId RefSupport x gameSupports ((.supportCardDef) . toAttrs)
  UpgradeRef x -> byId RefUpgrade x gameUpgrades getCardDef
  SideSchemeRef x -> byId RefSideScheme x gameSideSchemes ((.sideSchemeCardDef) . toAttrs)
  MainSchemeRef x -> byId RefMainScheme x gameMainSchemes getCardDef
  TreacheryRef x -> byId RefTreachery x gameTreacheries getCardDef
  AttachmentRef x -> byId RefAttachment x gameAttachments getCardDef
  EnvironmentRef x -> byId RefEnvironment x gameEnvironments getCardDef
  ObligationRef x -> byId RefObligation x gameObligations getCardDef
  EnemyRef (EnemyVillainId x) -> refFor (VillainRef x)
  EnemyRef (EnemyMinionId x) -> refFor (MinionRef x)
  SchemeRef (SchemeMainSchemeId x) -> refFor (MainSchemeRef x)
  SchemeRef (SchemeSideSchemeId x) -> refFor (SideSchemeRef x)
  CharacterRef c -> refFor case c of
    IdentityCharacter x -> IdentityRef x
    AllyCharacter x -> AllyRef x
    VillainCharacter x -> VillainRef x
    MinionCharacter x -> MinionRef x
  _ -> pure Nothing
 where
  byId :: (HasGame m, ToJSON i, Hashable i) => LogRefKind -> i -> (Game -> HashMap i a) -> (a -> CardDef) -> m (Maybe LogRef)
  byId kind x entities def = fmap (entityRef kind x . def) <$> getsGame (lookup x . entities)

villainPhaseStepKey :: VillainPhaseStep -> Text
villainPhaseStepKey = \case
  StepEnemiesActivate -> "log.step.enemiesActivate"
  StepDealEncounterCards -> "log.step.dealEncounterCards"
  StepRevealEncounterCards -> "log.step.revealEncounterCards"

redirected :: HasGame m => Target -> Target -> m [LogEntry]
redirected source target = whenRef source $ \enemy -> whenRef target $ \recipient ->
  one $ mechanic [ikeyPart "log.attackDamageRedirected" ["enemy" ~> enemy, "target" ~> recipient]]
