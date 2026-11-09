{-# LANGUAGE TemplateHaskell #-}

module Marvel.Game.Base where

import Data.Aeson.Diff qualified as Diff (Patch, diff, patch)
import Data.Aeson.KeyMap qualified as KeyMap (
  insert,
  insertWith,
  lookup,
 )
import Data.Aeson.TH (
  deriveJSON,
  mkParseJSON,
  mkToEncoding,
  mkToJSON,
 )
import Marvel.Ability.Types (Ability)
import Marvel.Ability.Types qualified as Ability (AbilitySubType)
import Marvel.ActiveCost.Types (ActiveCost)
import Marvel.Ally ()
import Marvel.Ally.Types (Ally)
import Marvel.Area (Area)
import Marvel.Attachment ()
import Marvel.Attachment.Types (Attachment)
import Marvel.Attack (Attack)
import Marvel.Campaign (Campaign)
import Marvel.Campaign qualified as Campaign (
  ShieldLog,
  noShieldLog,
 )
import Marvel.Card (
  Card,
  CardCode,
  CardDef,
  CardId,
  EncounterCard,
  PlayerCard,
  resolvePlayerCountCost,
 )
import Marvel.CivilWar.Types qualified as CW (
  Competition (competitiveRegistrationPlayers),
 )
import Marvel.Effect ()
import Marvel.Effect.Types (Effect)
import Marvel.Entity (Id)
import Marvel.Environment ()
import Marvel.Environment.Types (Environment)
import Marvel.Event ()
import Marvel.Event.Types (Event)
import Marvel.Id (
  AllyId,
  CharacterId,
  EnemyId,
  EnvironmentId,
  EventId,
  IdentityId,
  MinionId,
  SideSchemeId,
  TreacheryId,
  VillainId,
 )
import Marvel.Identity ()
import Marvel.Identity.Types (PlayerIdentity)
import Marvel.Keyword (Keyword (..))
import Marvel.MainScheme ()
import Marvel.MainScheme.Types (MainScheme)
import Marvel.Message (FinishedStatus, Message)
import Marvel.Minion ()
import Marvel.Minion.Types (Minion)
import Marvel.Obligation ()
import Marvel.Obligation.Types (Obligation)
import Marvel.Phase (Phase)
import Marvel.Prelude
import Marvel.Question (Question)
import Marvel.Ref (Source, Target)
import Marvel.Scenario ()
import Marvel.Scenario.Types (Scenario)
import Marvel.SideScheme ()
import Marvel.SideScheme.Types (SideScheme)
import Marvel.Support ()
import Marvel.Support.Types (Support)
import Marvel.Trait.Types (Trait)
import Marvel.Treachery ()
import Marvel.Treachery.Types (Treachery)
import Marvel.Upgrade ()
import Marvel.Upgrade.Types (Upgrade)
import Marvel.Villain ()
import Marvel.Villain.Types (Villain)
import Marvel.Window.Types (Window)

data GameState = Unstarted | InProgress | Finished FinishedStatus
  deriving stock (Show, Eq, Generic)

type EntityMap a = HashMap (Id a) a

data Entities = Entities
  { entitiesPlayers :: EntityMap PlayerIdentity
  , entitiesVillains :: EntityMap Villain
  , entitiesMinions :: EntityMap Minion
  , entitiesAllies :: EntityMap Ally
  , entitiesAttachments :: EntityMap Attachment
  , entitiesSupports :: EntityMap Support
  , entitiesUpgrades :: EntityMap Upgrade
  , entitiesTreacheries :: EntityMap Treachery
  , entitiesObligations :: EntityMap Obligation
  , entitiesMainSchemes :: EntityMap MainScheme
  , entitiesSideSchemes :: EntityMap SideScheme
  , entitiesEffects :: EntityMap Effect
  , entitiesEvents :: EntityMap Event
  , entitiesEnvironments :: EntityMap Environment
  }
  deriving stock (Show, Eq, Generic)

makeLensesWith suffixedFields ''Entities

defaultEntities :: Entities
defaultEntities =
  Entities
    { entitiesPlayers = mempty
    , entitiesVillains = mempty
    , entitiesMinions = mempty
    , entitiesAllies = mempty
    , entitiesAttachments = mempty
    , entitiesSupports = mempty
    , entitiesUpgrades = mempty
    , entitiesTreacheries = mempty
    , entitiesObligations = mempty
    , entitiesSideSchemes = mempty
    , entitiesMainSchemes = mempty
    , entitiesEffects = mempty
    , entitiesEvents = mempty
    , entitiesEnvironments = mempty
    }

data TrackedEnemyAttack = TrackedEnemyAttack
  { trackedAttackId :: CardId
  , trackedAttackEnemy :: EnemyId
  , trackedAttackSource :: Source
  , trackedAttackPrevious :: Maybe Attack
  , trackedAttackResult :: Maybe Attack
  }
  deriving stock (Show, Eq, Generic)

data SimultaneousMinionAttackGroup = SimultaneousMinionAttackGroup
  { simultaneousMinion :: MinionId
  , simultaneousGroupId :: CardId
  , simultaneousAttacks :: [(IdentityId, Attack, [EncounterCard], Natural, (Natural, Bool))]
  , simultaneousCurrent :: Int
  , simultaneousDefenders :: [CharacterId]
  , simultaneousPrevious :: Maybe SimultaneousMinionAttackGroup
  }
  deriving stock (Show, Eq, Generic)

data FriendlyBoostContext = FriendlyBoostContext
  { friendlyBoostAlly :: AllyId
  , friendlyBoostAttack :: Bool
  , friendlyBoostPending :: [EncounterCard]
  , friendlyBoostRevealed :: [EncounterCard]
  , friendlyBoostIcons :: Natural
  , friendlyBoostKeywords :: [Keyword]
  }
  deriving stock (Show, Eq, Generic)

data Game = Game
  { gamePrintedPlayerCounts :: [(Target, Int)]
  , gamePlayerCountContext :: Maybe Int
  , gameCompetition :: Maybe CW.Competition
  , gamePhase :: Phase
  , gameState :: GameState
  , -- players in player order
    gamePlayerOrder :: [IdentityId]
  , gameEntities :: Entities
  , gameBoostEntities :: Entities
  , gameRemovedFromGameEncounterCards :: [EncounterCard]
  , gameRemovedEntities :: Entities
  , gameQuestion :: HashMap IdentityId Question
  , gameUsedAbilities :: HashMap IdentityId [(Ability, Int)]
  , gameActivePlayer :: IdentityId
  , gamePayingForAbility :: Maybe (CardCode, Natural)
  , gameResolvingEvents :: Maybe [EventId]
  -- ^ Nothing only in legacy saves; restored from their pending completion messages.
  , gameResolvingAbilitySubTypes :: [(Source, [Ability.AbilitySubType], Bool)]
  -- ^ Active labeled ability scopes, innermost first; the flag marks a begun thwart.
  , gameYou :: Maybe IdentityId
  {- ^ who "you" is while a player's card, choice or window is looked at: its
  controller, or the player choosing or being offered it (see 'asYou'); otherwise
  the active player
  -}
  , gameBlankedAttackTargets :: [(Source, CharacterId)]
  , gameTrackedEnemyAttacks :: [TrackedEnemyAttack]
  , gameSimultaneousMinionAttack :: Maybe SimultaneousMinionAttackGroup
  , gameActiveCost :: [ActiveCost]
  , gameWindowDepth :: Int
  , gameDepth :: Int
  , gameWindows :: [[Window]]
  , gameScenario :: Scenario
  , gameFocusedCards :: [Card]
  , gamePrivateCards :: HashMap IdentityId [Card]
  -- ^ cards shown to one player only, as they look at them
  , gameBoostCards :: HashMap Target EncounterCard
  , gameQuestionVersion :: Int
  -- ^ bumped with each question asked, so a stale answer is refused
  , gameRound :: Int
  , gameSeats :: [IdentityId]
  {- ^ the players as they sat down, which never changes: the server finds whose seat is
  whose by it, while the player order turns as the first player passes
  -}
  , gamePlayedInWindows :: [[CardDef]]
  -- ^ cards played in each active window, including its nested windows
  , gamePlayedThisRound :: [(IdentityId, CardDef)]
  {- ^ the cards each player has played this round, for "max 1 per round" and "the first
  ally played each round"
  -}
  , gamePlayedThisPhase :: [(IdentityId, CardDef)]
  , gameAttacksThisTurn :: [(Source, EnemyId)]
  -- ^ completed attacks, including attacks which dealt no damage
  , gameHeroAttackedThisPhase :: [IdentityId]
  , gameHeroThwartedThisPhase :: [IdentityId]
  , gamePlayedThisTurn :: [(IdentityId, CardDef)]
  -- ^ the cards played since the current player's turn began
  , gameTreacheriesRevealedThisPhase :: [(IdentityId, TreacheryId)]
  -- ^ each treachery revealed this phase from the encounter deck, and for whom, in order
  , -- \^ and this phase, for "max 1 per phase"
    gameAreas :: [Area]
  -- ^ the game areas, the first the one every game starts with ("Marvel.Area")
  , gameSignatureSchemes :: HashMap VillainId SideSchemeId
  -- ^ each villain's own side scheme, as Wrecker's Day of Reckoning
  , gameAtEndOfPhase :: [Message]
  -- ^ what is to happen "at the end of the phase", in the order it was set to
  , gameMissionPlayDestination :: Maybe Bool
  , gameMissionTargets :: Bool
  , gameAllAreas :: Bool
  {- ^ whether what is looked at is looked at in every area, as whether a card matches
  is, wherever it is ('inAnyArea')
  -}
  , gameShieldLog :: Campaign.ShieldLog
  , gameCampaign :: Maybe Campaign
  {- ^ the campaign this game is a scenario of, as it was dealt and as the game writes in
  its log ("Marvel.Campaign")
  -}
  , gameVictoryDisplay :: [EncounterCard]
  , gamePlayerVictoryDisplay :: [PlayerCard]
  {- ^ the victory display: the out-of-play area shared by all players that cards with
  victory X go to as they are defeated (RR Victory Display), the first added first
  -}
  , gamePeril :: Maybe IdentityId
  -- ^ the player resolving a card with peril: the others cannot help them (RR Peril)
  , gameDefeatedMinionBoosts :: [(MinionId, EnemyId)]
  -- ^ Defeat interrupts choosing a boost destination after printed defeat effects.
  , gameInPlayBoosts :: [(MinionId, IdentityId, Minion)]
  -- ^ Scoped boost text resolutions on minions that remain in play, innermost first.
  , gameBoostPlayerOverride :: Maybe IdentityId
  -- ^ Query-only override while dispatching one in-play minion's boost text.
  , gameFriendlyBoosts :: [FriendlyBoostContext]
  , gameCancelledBoostIcons :: [CardId]
  , gamePlayerBoostCards :: [(CardId, PlayerCard)]
  -- ^ Original player cards temporarily represented by facedown boost cards.
  , gameCollection :: [Card]
  {- ^ The Collection (Infiltrate the Museum): an out-of-play area shared by all players,
  player and encounter cards faceup in it, the first added first
  -}
  , gameCollecting :: [CardId]
  -- ^ the cards in play on their way into The Collection, leaving play for it
  , gameUncontrolledAllies :: [AllyId]
  , gameAosHostages :: [(SideSchemeId, AllyId)]
  , gameAosBackup :: [(EnvironmentId, MinionId)]
  , gameCharacterThreat :: [(CharacterId, Natural)]
  , gameBombCounters :: [(EnemyId, Source, Natural)]
  , gameBoostAllyDefeatEffects :: [(EnemyId, Natural, Bool)]
  , gamePendingAttackDefeats :: [(AllyId, EnemyId)]
  , gameGainedTraits :: GainedTraits
  {- ^ the traits each character gains, worked out for the game as it stands
  ('refreshTraits')
  -}
  }
  deriving stock (Show, Eq, Generic)

{- | The traits each character gains from modifiers, or 'Nothing' until they are worked
out. Derived from the rest of the game, so it is never saved and never compared.
-}
newtype GainedTraits = GainedTraits (Maybe (HashMap Target (HashSet Trait)))

instance Show GainedTraits where
  show _ = "GainedTraits"

instance Eq GainedTraits where
  _ == _ = True

instance ToJSON GainedTraits where
  toJSON _ = Null

instance FromJSON GainedTraits where
  parseJSON _ = pure $ GainedTraits Nothing

$(concat <$> traverse (deriveJSON defaultOptions) [''GameState, ''Entities, ''TrackedEnemyAttack, ''SimultaneousMinionAttackGroup, ''FriendlyBoostContext])

makeLensesWith suffixedFields ''Game

gameAllies :: Game -> EntityMap Ally
gameAllies = entitiesAllies . gameEntities

gameAttachments :: Game -> EntityMap Attachment
gameAttachments = entitiesAttachments . gameEntities

gameEffects :: Game -> EntityMap Effect
gameEffects = entitiesEffects . gameEntities

gameEvents :: Game -> EntityMap Event
gameEvents = entitiesEvents . gameEntities

gameMinions :: Game -> EntityMap Minion
gameMinions = entitiesMinions . gameEntities

gamePlayers :: Game -> EntityMap PlayerIdentity
gamePlayers = entitiesPlayers . gameEntities

gameUpgrades :: Game -> EntityMap Upgrade
gameUpgrades = entitiesUpgrades . gameEntities

gameSupports :: Game -> EntityMap Support
gameSupports = entitiesSupports . gameEntities

gameMainSchemes :: Game -> EntityMap MainScheme
gameMainSchemes = entitiesMainSchemes . gameEntities

gameSideSchemes :: Game -> EntityMap SideScheme
gameSideSchemes = entitiesSideSchemes . gameEntities

gameTreacheries :: Game -> EntityMap Treachery
gameTreacheries = entitiesTreacheries . gameEntities

gameObligations :: Game -> EntityMap Obligation
gameObligations = entitiesObligations . gameEntities

gameVillains :: Game -> EntityMap Villain
gameVillains = entitiesVillains . gameEntities

gameEnvironments :: Game -> EntityMap Environment
gameEnvironments = entitiesEnvironments . gameEntities

instance ToJSON Game where
  toJSON = $(mkToJSON (aesonOptions $ Just "game") ''Game)
  toEncoding = $(mkToEncoding (aesonOptions $ Just "game") ''Game)

-- CardSpecific payloads and saved snapshots can contain physical cards nested in
-- arbitrary card metadata. Only their printed per-player definitions are changed.
resolveCardCostValues :: Int -> Value -> Value
resolveCardCostValues players = go
 where
  go (Array values) = Array $ fmap go values
  go (Object fields) =
    let nested = Object $ fmap go fields
        marked = case KeyMap.lookup "cdKeywords" fields of
          Just value
            | Success keywords <- fromJSON @[Keyword] value ->
                any (\case PerPlayerResourceCost _ -> True; _ -> False) keywords
          _ -> False
     in if marked
          then case fromJSON @CardDef nested of
            Success def -> toJSON $ resolvePlayerCountCost players def
            Error _ -> nested
          else nested
  go other = other

instance FromJSON Game where
  parseJSON = withObject "Game" $ \o -> do
    competitive <- o .:? "competition"
    let count key = case KeyMap.lookup key o of Just (Array xs) -> length xs; _ -> 0
        players = maybe (max 1 $ max (count "seats") (count "playerOrder")) (length . CW.competitiveRegistrationPlayers) competitive

    -- Saves from before phase limits were tracked have no phase history.
    $(mkParseJSON (aesonOptions $ Just "game") ''Game)
      $ resolveCardCostValues players
      $ Object
        ( KeyMap.insertWith (\_ existing -> existing) "competition" Null
            . KeyMap.insertWith (\_ existing -> existing) "printedPlayerCounts" (toJSON ([] :: [Value]))
            . KeyMap.insertWith (\_ existing -> existing) "playerCountContext" Null
            . KeyMap.insertWith (\_ existing -> existing) "shieldLog" (toJSON Campaign.noShieldLog)
            . KeyMap.insertWith (\_ existing -> existing) "missionPlayDestination" Null
            . KeyMap.insertWith (\_ existing -> existing) "missionTargets" (Bool False)
            . KeyMap.insertWith (\_ existing -> existing) "resolvingAbilitySubTypes" (toJSON ([] :: [Value]))
            . KeyMap.insertWith (\_ existing -> existing) "payingForAbility" Null
            . KeyMap.insertWith (\_ existing -> existing) "attacksThisTurn" (toJSON ([] :: [Value]))
            . KeyMap.insertWith (\_ existing -> existing) "heroAttackedThisPhase" (toJSON ([] :: [IdentityId]))
            . KeyMap.insertWith (\_ existing -> existing) "heroThwartedThisPhase" (toJSON ([] :: [IdentityId]))
            . KeyMap.insertWith (\_ existing -> existing) "playedThisPhase" (toJSON ([] :: [Value]))
            . KeyMap.insertWith (\_ existing -> existing) "treacheriesRevealedThisPhase" (toJSON ([] :: [Value]))
            . KeyMap.insertWith (\_ existing -> existing) "playedInWindows" (toJSON ([] :: [Value]))
            . KeyMap.insertWith (\_ existing -> existing) "playedThisTurn" (toJSON ([] :: [Value]))
            . KeyMap.insertWith (\_ existing -> existing) "removedFromGameEncounterCards" (toJSON ([] :: [Value]))
            . KeyMap.insertWith (\_ existing -> existing) "victoryDisplay" (toJSON ([] :: [Value]))
            . KeyMap.insertWith (\_ existing -> existing) "playerVictoryDisplay" (toJSON ([] :: [Value]))
            . KeyMap.insertWith (\_ existing -> existing) "defeatedMinionBoosts" (toJSON ([] :: [Value]))
            . KeyMap.insertWith (\_ existing -> existing) "inPlayBoosts" (toJSON ([] :: [Value]))
            . KeyMap.insertWith (\_ existing -> existing) "boostPlayerOverride" Null
            . KeyMap.insertWith (\_ existing -> existing) "friendlyBoosts" (toJSON ([] :: [Value]))
            . KeyMap.insertWith (\_ existing -> existing) "cancelledBoostIcons" (toJSON ([] :: [CardId]))
            . KeyMap.insertWith (\_ existing -> existing) "playerBoostCards" (toJSON ([] :: [Value]))
            . KeyMap.insertWith (\_ existing -> existing) "collection" (toJSON ([] :: [Value]))
            . KeyMap.insertWith (\_ existing -> existing) "collecting" (toJSON ([] :: [Value]))
            . KeyMap.insertWith (\_ existing -> existing) "aosHostages" (toJSON ([] :: [Value]))
            . KeyMap.insertWith (\_ existing -> existing) "aosBackup" (toJSON ([] :: [Value]))
            . KeyMap.insertWith (\_ existing -> existing) "uncontrolledAllies" (toJSON ([] :: [Value]))
            . KeyMap.insertWith (\_ existing -> existing) "characterThreat" (toJSON ([] :: [Value]))
            . KeyMap.insertWith (\_ existing -> existing) "blankedAttackTargets" (toJSON ([] :: [Value]))
            . KeyMap.insertWith (\_ existing -> existing) "trackedEnemyAttacks" (toJSON ([] :: [Value]))
            . KeyMap.insertWith (\_ existing -> existing) "simultaneousMinionAttack" Null
            . KeyMap.insertWith (\_ existing -> existing) "bombCounters" (toJSON ([] :: [Value]))
            . KeyMap.insertWith (\_ existing -> existing) "boostAllyDefeatEffects" (toJSON ([] :: [Value]))
            . KeyMap.insertWith (\_ existing -> existing) "pendingAttackDefeats" (toJSON ([] :: [Value]))
            . KeyMap.insert "gainedTraits" Null
            $ o
        )

diff :: Game -> Game -> Diff.Patch
diff a b = Diff.diff (toJSON a) (toJSON b)

patch :: Game -> Diff.Patch -> Result Game
patch g p = case Diff.patch p (toJSON g) of
  Error e -> Error e
  Success a -> fromJSON a
