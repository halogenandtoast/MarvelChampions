module Marvel.Game where

import Marvel.CivilWar.Types (Competition)
import Marvel.Prelude
import Marvel.AgeOfApocalypse.MissionTypes (MissionState)
import Marvel.Aspect (Aspect)
import Marvel.Status (StatusCard)
import Marvel.Card.Id (CardId)

import Control.Monad.IO.Class
import Control.Monad.Reader (ReaderT)
import Marvel.Ability.Types
import Marvel.Area (Area, AreaId, DeckKey, Pile)
import Marvel.Attack (Attack)
import Marvel.Damage (Damage)
import Marvel.Card.Code (CardCode)
import Marvel.Keyword (Keyword)
import {-# SOURCE #-} Marvel.Card.EncounterCard
import {-# SOURCE #-} Marvel.Card.Def (CardDef, CardMatcher)
import Marvel.Campaign (ShieldLog)
import Marvel.Campaign (Campaign)
import {-# SOURCE #-} Marvel.Question (Choice)
import {-# SOURCE #-} Marvel.Message (Message)
import Marvel.Card.PlayerCard.Types
import Marvel.Card.Types (Card, IsCard)
import Marvel.Count
import Marvel.Cost.Types (Cost)
import Marvel.Debug
import Marvel.Difficulty
import Marvel.Id
import Marvel.Identity.Types
import Marvel.Matchers.Types
import Marvel.Modifier.Types
import Marvel.Payment.Types
import Marvel.Phase (Phase)
import Marvel.Projection
import Marvel.Queue
import Marvel.Ref
import Marvel.Resource.Types
import Marvel.Trait.Types (Trait)
import Marvel.Window.Types

class (Monad m) => HasGame m where
  getGame :: m Game

class HasGameRef m where
  getGameRef :: m (IORef Game)

class
  ( MonadCatch m
  , MonadThrow m
  , MonadIO m
  , MonadReader env m
  , HasGame m
  , HasGameRef m
  , HasQueue m
  , HasDebugLogger env
  , MonadRandom m
  , CoerceRole m
  ) =>
  MonadGame env m
    | env -> m
    , m -> env

data Game

instance (Monad m) => HasGame (ReaderT Game m)
instance (HasQueue m) => HasQueue (ReaderT Game m)
instance (HasGame m) => Projection m PlayerIdentity

getPlayers :: (HasGame m) => m [IdentityId]
getAllPlayers :: (HasGame m) => m [IdentityId]
getCompetition :: HasGame m => m (Maybe Competition)
getPhasePlayers :: HasGame m => m [IdentityId]
getPhaseAreas :: HasGame m => m [Area]
getActivePlayerId :: (HasGame m) => m IdentityId
getYou :: (HasGame m) => m IdentityId
asYou :: (HasGame m) => IdentityId -> ReaderT Game m a -> m a
asPayingForAbility :: HasGame m => Ability -> ReaderT Game m a -> m a
inAnyArea :: (HasGame m) => ReaderT Game m a -> m a
getPlayerCount :: (HasGame m) => m Int
getStartingPlayerCount :: (HasGame m) => m Int
resolvePlayerCardDef :: HasGame m => CardDef -> m CardDef
resolvePlayerCardCosts :: HasGame m => PlayerCard -> m PlayerCard
lookupPlayerCardInGame :: HasGame m => CardCode -> m CardDef
getPlayerCardCatalogue :: HasGame m => m (HashMap CardCode CardDef)
getsGame :: (HasGame m) => (Game -> a) -> m a
getCampaign :: (HasGame m) => m (Maybe Campaign)
offerable :: Game -> Choice -> Bool
getIdentityCode :: (HasGame m) => IdentityId -> m CardCode
getRemainingHitPoints :: (HasGame m) => IdentityId -> m (Natural, Natural, Natural)
getUpgradeAttachedSchemes :: (HasGame m) => UpgradeId -> m [SchemeId]
getCountersOnMainSchemes :: (HasGame m) => m Natural
getSideSchemesInPlay :: (HasGame m) => m [(CardCode, [PlayerCard])]
getCardsOwnedBy :: (HasGame m) => IdentityId -> m [CardDef]
abilityCardDef :: Game -> Target -> Maybe CardDef
cardInPlay :: Game -> Target -> Maybe Card
getUsedAbilities :: (HasGame m) => m (HashMap IdentityId [Ability])
withinUseLimit :: (HasGame m) => IdentityId -> Ability -> m Bool
getPlayedThisRound :: (HasGame m) => m [(IdentityId, CardDef)]
getPlayedThisPhase :: (HasGame m) => m [(IdentityId, CardDef)]
getAttackTargetsThisTurn :: (HasGame m) => Source -> m [EnemyId]
getPlayedThisTurn :: (HasGame m) => m [(IdentityId, CardDef)]
treacheryHasResolvedPart :: (HasGame m) => TreacheryId -> m Bool
getTreacheriesRevealedThisPhase :: (HasGame m) => m [(IdentityId, TreacheryId)]
getPhase :: (HasGame m) => m Phase
getEncounterDeck :: (HasGame m) => m [EncounterCard]
getBoostIcons :: (HasGame m) => EncounterCard -> m Natural
getEncounterDiscard :: (HasGame m) => m [EncounterCard]
getAbilitiesFor :: (HasGame m) => IdentityId -> m [Ability]
getHeroSourceController :: (HasGame m) => Source -> m (Maybe IdentityId)
sourceATKAttackIdentity :: HasGame m => Source -> m (Maybe IdentityId)
sourceHasBegunLabeledAttack :: (HasGame m) => Source -> m Bool
sourceHasAttackedTarget :: HasGame m => Source -> EnemyId -> m Bool

getAllyControlSource :: (HasGame m) => AllyId -> m (Maybe Source)

getCharacterOwner :: (HasGame m) => CharacterId -> m (Maybe IdentityId)

getSourceController :: (HasGame m) => Source -> m (Maybe IdentityId)
isPlayerCardSource :: (HasGame m) => Source -> m Bool
cannotTakeDamageFrom :: (HasGame m) => Target -> Source -> m Bool
-- Matchers
gameSelectIdentity :: (HasGame m) => IdentityMatcher -> m (HashSet IdentityId)
gameSelectEnemy :: (HasGame m) => EnemyMatcher -> m (HashSet EnemyId)
gameSelectVillain :: (HasGame m) => VillainMatcher -> m (HashSet VillainId)
gameSelectMinion :: (HasGame m) => MinionMatcher -> m (HashSet MinionId)
gameSelectAlly :: (HasGame m) => AllyMatcher -> m (HashSet AllyId)
gameSelectSupport :: (HasGame m) => SupportMatcher -> m (HashSet SupportId)
gameSelectUpgrade :: (HasGame m) => UpgradeMatcher -> m (HashSet UpgradeId)
gameSelectAttachment ::
  (HasGame m) => AttachmentMatcher -> m (HashSet AttachmentId)
gameSelectEnvironment ::
  (HasGame m) => EnvironmentMatcher -> m (HashSet EnvironmentId)
gameSelectScheme :: (HasGame m) => SchemeMatcher -> m (HashSet SchemeId)
gameSelectSideScheme ::
  (HasGame m) => SideSchemeMatcher -> m (HashSet SideSchemeId)
gameSelectTreachery :: (HasGame m) => TreacheryMatcher -> m (HashSet TreacheryId)
gameSelectCharacter :: (HasGame m) => CharacterMatcher -> m (HashSet CharacterId)
gameSelectExtendedCard ::
  ( MonadThrow m
  , HasGame m
  , HasQueue m
  , Projection m PlayerIdentity
  ) =>
  ExtendedCardMatcher ->
  m (HashSet PlayerCard)
gameSelectEncounterCard ::
  (MonadRandom m, HasGame m) =>
  EncounterCardMatcher ->
  m (HashSet EncounterCard)

instance HasAbilities Game

resourceCostPayers :: (HasGame m) => IdentityId -> Maybe PlayerCard -> m [IdentityId]
getPaymentSourcesFor ::
  (HasQueue m, MonadThrow m, HasGame m, Projection m PlayerIdentity) =>
  IdentityId -> IdentityId -> Maybe PlayerCard -> m [PlayerCard]
getPaymentResourceAbilities ::
  (MonadThrow m, MonadRandom m, HasQueue m, HasGame m) =>
  IdentityId -> IdentityId -> Maybe PlayerCard -> m [Ability]
getAvailableResourcesFor ::
  ( HasQueue m
  , MonadRandom m
  , MonadThrow m
  , HasGame m
  , Projection m PlayerIdentity
  ) =>
  IdentityId ->
  Maybe PlayerCard ->
  m [Resource]
isBlanked :: (HasGame m, IsRef a) => a -> m Bool

discardableControlledCardsMatching :: (HasGame m) => IdentityId -> CardMatcher -> m [Target]

getModifiers :: (HasGame m, IsRef a) => a -> m [Modifier]
getGainedTraits :: (HasGame m, IsRef a) => a -> m (HashSet Trait)
getUpgradeHeldCards :: (HasGame m) => IdentityId -> m [PlayerCard]
getPlayTargets ::
  (HasGame m, HasQueue m, MonadThrow m, Projection m PlayerIdentity) =>
  IdentityId ->
  PlayerCard ->
  m (Maybe [Target])
getCurrentWindows :: (HasGame m) => m [Window]
getCurrentPayment :: (HasGame m) => m Payment
getActiveCost :: (HasGame m) => m (Maybe ActiveCostId)
getDifficulty :: (HasGame m) => m Difficulty
getUniqueTitlesInPlay :: (HasGame m) => m [(Text, [Text])]
getVillainActivationPlayer :: (HasGame m) => VillainId -> m (Maybe IdentityId)
getYourMainSchemeCounters :: (HasGame m) => m Natural
getEnvironmentTraits :: (HasGame m) => m (HashSet Trait)
getCountersOn :: (HasGame m, IsRef a) => a -> m Natural

getVillainAttack :: (HasGame m) => VillainId -> m (Maybe Attack)
getScenarioCode :: (HasGame m) => m CardCode
getVillainAttackValue :: (HasGame m) => VillainId -> m Natural
getVillainSchemeValue :: (HasGame m) => VillainId -> m Natural
getPairedMainScheme :: (HasGame m) => VillainId -> m (Maybe MainSchemeId)
getExcessDamage :: (HasGame m) => CharacterId -> Natural -> m Natural
countCardsInPlayWithTrait :: (HasGame m) => Trait -> m Natural
whenRevealedTimes :: (HasGame m) => IdentityId -> m Int
getBoostedPlayer :: (HasGame m) => EnemyId -> m IdentityId
countObligationsOf :: (HasGame m) => IdentityId -> m Natural
getVillainStage :: (HasGame m) => VillainId -> m Natural
villainPreventsAttackDamage :: (HasGame m) => VillainId -> Source -> Damage -> m Bool
getVillainHitPoints :: (HasGame m) => VillainId -> m Natural
getEnemyPrintedHitPoints :: (HasGame m) => EnemyId -> m Natural
getHazardCount :: (HasGame m) => m Natural
getEncounterIconCounts :: (HasGame m) => m (Natural, Natural, Natural, Natural)
getEncounterIconCount :: (HasGame m) => m Natural
getHazardCountIn :: (HasGame m) => AreaId -> m Natural
getAccelerationCount :: (HasGame m) => m Natural
getAccelerationCountIn :: (HasGame m) => AreaId -> m Natural
getAmplifyCount :: (HasGame m) => m Natural
-- The Galaxy's Most Wanted
getVictoryDisplay :: (HasGame m) => m [EncounterCard]
getPlayerVictoryDisplay :: (HasGame m) => m [PlayerCard]
getVictoryPoints :: (HasGame m) => m Int
getCollection :: (HasGame m) => m [Card]
getMilano :: (HasGame m) => m (Maybe SupportId)
milanoIsReady :: (HasGame m) => m Bool
controlsMilano :: (HasGame m) => IdentityId -> m Bool
getPowerStone :: (HasGame m) => m (Maybe AttachmentId)
getAttachedCharacter :: (HasGame m) => AttachmentId -> m (Maybe CharacterId)
getPowerStoneHolder :: (HasGame m) => m (Maybe CharacterId)
controlsPowerStone :: (HasGame m) => IdentityId -> m Bool
isStalwartEnemy :: (HasGame m) => Target -> m Bool
-- Areas
getAreas :: (HasGame m) => m [Area]
getAreaOf :: (HasGame m) => IdentityId -> m Area
getAreaOfTarget :: (HasGame m, IsRef a) => a -> m AreaId
getYourArea :: (HasGame m) => m Area
inYourArea :: (HasGame m) => Target -> m Bool
getVillainsInArea :: (HasGame m) => AreaId -> m [VillainId]
getActiveVillainIn :: (HasGame m) => AreaId -> m (Maybe VillainId)
getDeckKeyFor :: (HasGame m) => IdentityId -> m DeckKey
getMainSchemesIn :: (HasGame m) => AreaId -> m [MainSchemeId]
getMinionsIn :: (HasGame m) => AreaId -> m [MinionId]
getObligationsIn :: (HasGame m) => AreaId -> m [ObligationId]
getObligationsOf :: HasGame m => IdentityId -> m [ObligationId]
getSideSchemesIn :: (HasGame m) => AreaId -> m [SideSchemeId]
getMainSchemeCode :: (HasGame m) => MainSchemeId -> m (Maybe CardCode)
getNemesisMinionsOutOfPlay :: (HasGame m) => IdentityId -> m [EncounterCard]
isPatrolled :: (HasGame m) => IdentityId -> m Bool
getUniqueCardsIn :: (HasGame m) => AreaId -> m [(Target, (Text, [Text]))]
getPile :: (HasGame m) => DeckKey -> m Pile
getDeckKeys :: (HasGame m) => m [DeckKey]

getMainSchemeCounters :: (HasGame m) => MainSchemeId -> m Natural

getSetAsideEncounterCards :: (HasGame m) => m [EncounterCard]

getSetAsidePlayerCards :: (HasGame m) => m [PlayerCard]
getAvailablePaymentSources ::
  (HasQueue m, MonadThrow m, HasGame m, Projection m PlayerIdentity) => IdentityId -> m [PlayerCard]
-- Adam Warlock
pendingDamageAmount :: (HasQueue m) => IdentityId -> m Natural
getResourceAbilities ::
  ( MonadThrow m
  , MonadRandom m
  , HasQueue m
  , HasGame m
  ) =>
  IdentityId ->
  Maybe PlayerCard ->
  m [Ability]
abilityResources ::
  (HasQueue m, MonadThrow m, HasGame m) => Ability -> m [Resource]
abilityResourcesWithReservedCounters ::
  (HasQueue m, MonadThrow m, HasGame m) => Natural -> Ability -> m [Resource]
gameSelectCountScheme ::
  (HasGame m) => QueryCount SchemeMatcher -> SchemeMatcher -> m Natural
gameSelectCountIdentity ::
  (HasGame m) => QueryCount IdentityMatcher -> IdentityMatcher -> m Natural
gameSelectCountAlly ::
  (HasGame m) => QueryCount AllyMatcher -> AllyMatcher -> m Natural
-- Hela
sideSchemesInVictoryDisplay :: (HasGame m) => m Natural
getMainSchemeHeldCards :: (HasGame m) => MainSchemeId -> m [PlayerCard]

abilityResourcesForCardWithReservedCounters :: (HasQueue m, MonadThrow m, HasGame m) => Maybe PlayerCard -> Natural -> Ability -> m [Resource]

getAllyHeldCards :: (HasGame m) => IdentityId -> m [PlayerCard]
allianceCostPayers :: (HasGame m) => IdentityId -> Maybe PlayerCard -> m [IdentityId]

getAllyHoldingCard :: (HasGame m) => PlayerCard -> m (Maybe AllyId)

effectivePlayerCardDef :: (HasGame m) => IdentityId -> PlayerCard -> m CardDef

getIdentityAttackAgainst :: (HasGame m) => IdentityId -> EnemyId -> m Natural
attachmentHasFriendlyHost :: (HasGame m) => AttachmentId -> m Bool

playableClawsEvents :: (HasGame m, HasQueue m) => IdentityId -> Source -> m [PlayerCard]

canPlayCardIgnoringResourceCost :: (HasGame m, HasQueue m) => IdentityId -> PlayerCard -> m Bool

canPlayCardWithReduction :: (HasGame m, HasQueue m) => IdentityId -> Natural -> PlayerCard -> m Bool
playableCardsWithReductionAfterExhausting :: (HasGame m, HasQueue m) => IdentityId -> Target -> CardMatcher -> Natural -> m [PlayerCard]
hasCardToPlayWithReductionAfterExhausting :: (HasGame m, HasQueue m) => IdentityId -> PlayerCard -> Natural -> m Bool

getActiveCostCard :: (HasGame m) => ActiveCostId -> m (Maybe PlayerCard)

getActiveCostPayer :: (HasGame m) => ActiveCostId -> m (Maybe IdentityId)
eventExhaustionsValid :: (HasGame m) => IdentityId -> PlayerCard -> [CharacterId] -> m Bool

eventExhaustionsAffordable :: (HasGame m) => IdentityId -> [IdentityId] -> PlayerCard -> Cost -> m Bool

sourceAbilitySubTypes :: (HasGame m) => Source -> m [AbilitySubType]

getSupportHoldingCard :: (HasGame m) => PlayerCard -> m (Maybe SupportId)
getSupportHeldCards :: (HasGame m) => IdentityId -> m [PlayerCard]
getSupportHeldCardsOn :: (HasGame m) => SupportId -> m [PlayerCard]
isCurrentBoostCard :: (HasGame m) => EncounterCard -> m Bool
getBoostCard :: (HasGame m) => Target -> m (Maybe EncounterCard)
characterAttackValue :: (HasGame m) => CharacterId -> m Natural
countControlledCardsWithTrait :: (HasGame m) => IdentityId -> Trait -> m Natural
getHeldPlayableCards :: (HasGame m) => IdentityId -> m [PlayerCard]

getPlayedThisWindow :: (HasGame m) => m [CardDef]

supportIsInPlay :: (HasGame m) => SupportId -> m Bool

indirectCostCandidates :: (HasGame m) => IdentityId -> Source -> m [(CharacterId, Natural)]
characterStoredDamage :: (HasGame m) => CharacterId -> m Natural

hasCrisisInYourArea :: (HasGame m) => m Bool
canPayAdditionalThwartCost :: (HasGame m) => IdentityId -> SchemeId -> m Bool
basicThwartSchemes :: (HasGame m) => IdentityId -> m (HashSet SchemeId)

encounterCardCanBeCanceled :: (HasGame m) => EncounterCard -> m Bool

getGliderSchemes :: (HasGame m) => m [MainSchemeId]
isRestrictedMainScheme :: (HasGame m) => MainSchemeId -> m Bool

getMainSchemeAccelerationTokensIn :: (HasGame m) => AreaId -> m Natural
getMainSchemeTokenCountIn :: (HasGame m) => AreaId -> m Natural
getMainSchemeTokenCount :: (HasGame m) => m Natural
getAccelerationTokensOnMainScheme :: (HasGame m) => MainSchemeId -> m Natural

getRemovedFromGameEncounterCards :: (HasGame m) => m [EncounterCard]

getAccelerationTokenCount :: (HasGame m) => m Natural

replacePowerWithDiscard :: (HasGame m, HasQueue m, MonadThrow m, MonadRandom m) => IdentityId -> m Bool

replaceHeroAttackWithDiscardAndStun :: (HasGame m, HasQueue m, MonadThrow m, MonadRandom m) => IdentityId -> m Bool

obligationIsInPlay :: (HasGame m) => ObligationId -> m Bool

getEventPlayerCard :: HasGame m => EventId -> m (Maybe PlayerCard)
getEventPaidResources :: (HasGame m) => EventId -> m [Resource]

isIdentityDefending :: (HasGame m) => IdentityId -> m Bool

getZoneHeldCards :: (HasGame m) => Target -> m [PlayerCard]

getEnemyKeywords :: HasGame m => EnemyId -> m [Keyword]
getEnemyAttackValue :: HasGame m => EnemyId -> m Natural
getEnemyScheme :: HasGame m => EnemyId -> m Natural
getEnemyBaseScheme :: HasGame m => EnemyId -> m Natural
getEnemyPrintedScheme :: HasGame m => EnemyId -> m Natural
getEnemyPrintedAttack :: (HasGame m) => EnemyId -> m Natural
getAttachmentIdentity :: (HasGame m) => AttachmentId -> m (Maybe IdentityId)

usesAttackForDefense :: HasGame m => IdentityId -> EnemyId -> m Bool

canMoveSchemeThreatFrom :: (HasGame m, IsRef source) => source -> SchemeId -> m Bool

weatherSpecialCanResolve :: (HasGame m) => IdentityId -> SupportId -> m Bool

sourceHasBegunLabeledThwart :: HasGame m => Source -> m Bool

withAdditionalIdentityCounters :: HasGame m => [(IdentityId, Natural)] -> ReaderT Game m a -> m a
recalculateCardPaymentCost :: (HasGame m, MonadThrow m, Projection m PlayerIdentity) => IdentityId -> PlayerCard -> Payment -> m Cost

canFindTouched :: HasGame m => IdentityId -> m Bool
touchedHost :: (HasGame m) => IdentityId -> m (Maybe CharacterId)
getCharacterTraits :: (HasGame m) => CharacterId -> m (HashSet Trait)

getCharacterThreat :: (HasGame m) => CharacterId -> m Natural
transferMojoThreat :: (HasGame m, HasQueue m, MonadRandom m, MonadThrow m) => CharacterId -> m ()

canPlayCardIgnoringResourceCostFromAnyZone :: (HasGame m, HasQueue m) => IdentityId -> PlayerCard -> m Bool

canPlayHeldAlly :: (HasGame m, HasQueue m) => IdentityId -> PlayerCard -> m Bool
heldAllyPlayable :: (HasGame m, HasQueue m) => IdentityId -> SupportId -> m Bool

getRevealingCardModifiers :: (HasGame m, IsRef a, IsCard a) => a -> m [Modifier]

canCaptureDefeatedAlly :: HasGame m => PlayerCard -> m Bool

discardResponseAvailable :: HasGame m => CardId -> m Bool


characterThwartValue :: HasGame m => CharacterId -> m Natural
canAttachPlayerUpgradeToAlly :: HasGame m => AllyId -> PlayerCard -> m Bool



canPlayerRemoveThreatFrom :: HasGame m => IdentityId -> SchemeId -> m Bool
canRemoveThreatFrom :: HasGame m => Source -> SchemeId -> m Bool
canPlayerDamageEnemy :: HasGame m => IdentityId -> EnemyId -> m Bool


incomingEnemyDamage :: HasGame m => EnemyId -> Source -> Damage -> m Damage

canSourceDamageEnemy :: HasGame m => Source -> EnemyId -> m Bool

enemyAttackTargetsIdentity :: HasGame m => EnemyId -> IdentityId -> m Bool
getEnemyAttack :: HasGame m => EnemyId -> m (Maybe Attack)


getEnemyRemainingHitPoints :: HasGame m => EnemyId -> m Natural
getCharacterStatusCount :: HasGame m => CharacterId -> StatusCard -> m Natural


canDamageThroughAerialRestriction :: HasGame m => Target -> Source -> Damage -> m Bool
friendlyCharactersWithFewestRemainingHitPoints :: HasGame m => m [CharacterId]
characterToughCount :: HasGame m => CharacterId -> m Natural


getCharacterRemainingHitPoints :: HasGame m => CharacterId -> m Natural

getControlledPlayerSideSchemes :: HasGame m => IdentityId -> Bool -> m [SideSchemeId]

getIdentityAttackedAndThwartedThisPhase :: HasGame m => IdentityId -> m Bool

schemeHasKeyword :: HasGame m => SchemeId -> Keyword -> m Bool

characterHasCondition :: HasGame m => StatusCard -> CharacterId -> m Bool

getAttacksThisTurn :: (HasGame m) => m [(Source, EnemyId)]

getPreviousGameWon :: HasGame m => IdentityId -> m (Maybe Bool)
poolIconEffectsCanResolve :: HasGame m => IdentityId -> Source -> m Bool
getLegalHeldWeaponSwaps ::
  (HasGame m, HasQueue m, MonadThrow m, Projection m PlayerIdentity) =>
  IdentityId -> UpgradeId -> m [(UpgradeId, PlayerCard)]
upgradeSlotIsEmpty :: HasGame m => UpgradeId -> Int -> m Bool

canResolvePlayerCardAbilities :: HasGame m => IdentityId -> m Bool

prepareEnemyAttack :: HasGame m => EnemyId -> Attack -> m Attack

controlsAspectCard :: HasGame m => IdentityId -> Aspect -> m Bool
characterHasAttachment :: HasGame m => CharacterId -> CardDef -> m Bool

getPlayableHandCards :: HasGame m => IdentityId -> m [PlayerCard]
isPlayableHandCard :: HasGame m => IdentityId -> PlayerCard -> m Bool

capIdentityAttackDamage :: HasGame m => IdentityId -> Source -> Damage -> m Damage

getMission :: HasGame m => m (Maybe MissionState)

findOwnedPlayerCard :: HasGame m => IdentityId -> CardCode -> m (Maybe PlayerCard)

withMissionPlayDestination :: HasGame m => Bool -> ReaderT Game m a -> m a
missionDestinationPreview :: HasGame m => m Bool
missionAllyReduction :: HasGame m => PlayerCard -> m Natural

getSchemeThreatAmount :: HasGame m => SchemeId -> m Natural
getMissionAllyStats :: HasGame m => AllyId -> m (Natural, Natural)
getAllyAttachedUpgrades :: HasGame m => AllyId -> m [UpgradeId]

playerCardEntryAllowed :: HasGame m => PlayerCard -> m Bool

findEncounterCards :: HasGame m => CardMatcher -> m [EncounterCard]

canResolveSettingSpecial :: HasGame m => IdentityId -> m Bool

getStartingHeroCodes :: HasGame m => m [CardCode]

getCharacterBasePowers :: HasGame m => CharacterId -> m (Natural, Natural)
getCharacterThwart :: HasGame m => CharacterId -> m Natural
getUpgradeAttachedEnemy :: HasGame m => UpgradeId -> m (Maybe EnemyId)
getMinionEngagedPlayer :: HasGame m => MinionId -> m (Maybe IdentityId)
getAllyControllerId :: HasGame m => AllyId -> m (Maybe IdentityId)

getAbilitiesForMatching :: HasGame m => IdentityId -> (Ability -> m Bool) -> m [Ability]

getCharacterThwartAgainstScheme :: HasGame m => CharacterId -> SchemeId -> m Natural

getAllyPrintedStats :: HasGame m => AllyId -> m (Natural, Natural, Natural)

getShieldLog :: HasGame m => m ShieldLog

allyDefeatedByConsequential :: HasGame m => AllyId -> m Bool

organizationalSupportCandidates :: HasGame m => IdentityId -> m [(Target,[Resource])]
organizationalSupportPotential :: HasGame m => IdentityId -> m [Resource]

conditionalAttackReduction :: HasGame m => Target -> Source -> Damage -> m Natural

attackableAttachmentTargets :: HasGame m => EnemyMatcher -> m [EnemyId]

discardIsFromEncounter :: HasGame m => m Bool

getAssignedBoostInWindow :: HasGame m => EnemyId -> EncounterCard -> m (Maybe EncounterCard)

getMinionDamageInPlay :: HasGame m => MinionId -> m Natural

canAllyDefendEnemy :: HasGame m => EnemyId -> AllyId -> m Bool

attachmentIsReady :: HasGame m => AttachmentId -> m Bool

getActiveCostValue :: HasGame m => ActiveCostId -> m (Maybe Cost)
getCardsInPlayTargets :: HasGame m => m [Target]

encounterCardPrintedAttack :: IdentityId -> EncounterCard -> Natural
encounterCardPrintedScheme :: IdentityId -> EncounterCard -> Natural

encounterCardPrintedHitPoints :: IdentityId -> EncounterCard -> Natural

characterAttackDamageAllowed :: HasGame m => Target -> Damage -> m Bool

herculesCanRevealLaborFor :: HasGame m => IdentityId -> m Bool
herculesHasGiftFor :: HasGame m => IdentityId -> m Bool

canPlayTopSenseFor :: (HasGame m, HasQueue m) => IdentityId -> m Bool
hasEchoTuckedEventFor :: HasGame m => IdentityId -> m Bool

canAffordEchoReplayFor :: (HasGame m, HasQueue m) => Bool -> IdentityId -> PlayerCard -> m Bool

getEffectiveHerculesGiftCount :: HasGame m => IdentityId -> m Natural

findPlayerCardsForEntry :: HasGame m => IdentityId -> CardCode -> m [PlayerCard]

canPlaySearchedCardWithReduction :: (HasGame m, HasQueue m) => IdentityId -> Natural -> PlayerCard -> m Bool

getAllyPhysicalCard :: HasGame m => AllyId -> m (Maybe Card)

allyMatchesAtDefeat :: HasGame m => AllyMatcher -> AllyId -> m Bool

competitiveBoardFinishedFor :: Game -> IdentityId -> Bool

canPutPlayerAllyIntoPlay :: HasGame m => IdentityId -> PlayerCard -> m Bool

getSetAsideEncounterCardsForDeck :: HasGame m => DeckKey -> m [EncounterCard]

capEnemyAttackDamageTaken :: HasGame m => EnemyId -> Source -> Damage -> m Damage

characterCanGainStatus :: HasGame m => CharacterId -> StatusCard -> m Bool

getTargetCardDef :: HasGame m => Target -> m (Maybe CardDef)

mainSchemeMatcherForSource :: HasGame m => IdentityId -> Source -> SchemeMatcher -> m SchemeMatcher
getOwnedMainScheme :: HasGame m => IdentityId -> m (Maybe MainSchemeId)

characterPlayAreaOwner :: HasGame m => CharacterId -> m (Maybe IdentityId)
cardPlayAreaOwner :: HasGame m => Target -> m (Maybe IdentityId)
threatSourceCharacter :: HasGame m => Source -> m (Maybe CharacterId)

boostAssignmentEnemy :: Message -> Maybe EnemyId

fearNoEvilRemovedTitles :: HasGame m => m [Text]

getRound :: HasGame m => m Int
