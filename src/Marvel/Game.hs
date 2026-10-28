{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Marvel.Game where

import Marvel.Prelude
import Marvel.GameValue (fromGameValue)

import Control.Monad (replicateM_)
import Control.Monad.Reader
import Data.Aeson.Diff qualified as Diff
import Data.Aeson.KeyMap qualified as KeyMap
import Data.HashMap.Strict qualified as HashMap
import Data.HashSet qualified as HashSet
import Data.IORef
import Data.Tuple (swap)
import Marvel.Frontend
import System.Random (StdGen, mkStdGen, random, randomR, randomRs, randoms, splitGen)
import Data.List (find, nub, partition)
import Marvel.Ability
import Marvel.Ability qualified as Ability
import Marvel.ActiveCost
import Marvel.Ally
import Marvel.Area
import Marvel.Ally.Types (allyOwner, allyUpgrades, getAllyController, getAllyDamage, getAllyUses)
import Marvel.Ally.Types qualified as Ally
import Marvel.AlterEgo.Cards
import Marvel.Attachment
import Marvel.Attachment.Types (Attachment, Attrs (attachmentSideScheme), Field (AttachmentCounters))
import Marvel.Attachment.Types qualified as Attachment
import Marvel.Attack
import Marvel.Campaign (Campaign, CampaignStepKind (..))
import Marvel.Campaign qualified as Campaign
import Marvel.Card
import Marvel.Card.WhenRevealed (hasWhenRevealed)
import Marvel.Choice (choiceMessages)
import Marvel.Cost
import Marvel.Damage
import Marvel.Count
import Marvel.Criteria
import Marvel.Debug
import Marvel.Deck
import Marvel.Difficulty
import Marvel.Discard
import Marvel.Effect
import Marvel.Effect.Types (Effect)
import Marvel.EncounterSet
import Marvel.Environment
import Marvel.Environment.Types (Environment, Field (EnvironmentCounters))
import Marvel.Entity
import Marvel.Event
import Marvel.Event.Types (Attrs (eventCardDef, eventController), Event (..), eventTargets)
import Marvel.Exception
import Marvel.Hand
import Marvel.Hero.Cards
import Marvel.Hp
import Marvel.Id
import Marvel.Identity hiding (alliesL, minionsL, supportsL, upgradesL)
import Marvel.Keyword
import Marvel.MainScheme
import Marvel.MainScheme.Types (Attrs (mainSchemeCounters), Field (MainSchemeCounters), MainScheme, getMainSchemeThreat)
import Marvel.Matchers hiding (ExhaustedIdentity, MainScheme)
import Marvel.Matchers qualified as Matchers
import Marvel.Message.Lifted hiding (ExhaustedAlly, createEffect)
import Marvel.Minion
import Marvel.Minion.Types qualified as Minion
import Marvel.Minion.Cards qualified as Minions
import Marvel.Minion.Types (
  Attrs (minionConfused, minionStunned, minionTough),
  Minion,
  facedownCardL,
  overMinion,
  getMinionDamage,
  getMinionEngagedIdentity,
  getMinionPrintedHitPoints,
  minionAttachments,
  minionAttackDetails,
  minionUpgrades,
  minionEngagedIdentity,
 )
import Marvel.Modifier
import Marvel.Name
import Marvel.Obligation
import Marvel.Obligation.Types (Attrs (obligationOwner), Obligation)
import Marvel.Obligation.Types qualified as Obligation
import Marvel.Payment
import Marvel.Phase
import Marvel.Projection
import Marvel.Query
import Marvel.Question
import Marvel.Boost (boostCount)
import Marvel.Queue
import Marvel.Ref
import Marvel.Resource
import Marvel.Scenario
import Marvel.Scenario.Types (Attrs (scenarioDecks, scenarioDiscard, scenarioEncounterDeck), Scenario, deckKeys, getScenarioDifficulty, scenarioSetAsideCards)
import Marvel.SideScheme
import Marvel.SideScheme.Types (Attrs (sideSchemeHeldCards), SideScheme, getSideSchemeThreat, isCrisis)
import Marvel.Support
import Marvel.Support.Types (Attrs (supportCardDef, supportHeldCards), Support, getSupportController, getSupportUses)
import Marvel.Trait
import Marvel.Treachery
import Marvel.Treachery.Types (Treachery)
import Marvel.Upgrade
import Marvel.Upgrade.Types (Attrs (upgradeAttachedAlly, upgradeAttachedEnemy, upgradeAttachedMainScheme, upgradeAttachedSideScheme, upgradeHeldCards, upgradeOwner), Upgrade (..), getUpgradeController, getUpgradeUses, upgradeTargets)
import Marvel.Villain
import Marvel.Villain.Types qualified as Villain
import Marvel.Villain.Types (
  Villain,
  villainAttackDetails,
  villainDamage,
  villainIsTough,
  villainUpgrades,
 )
import Marvel.Window (Window, WindowTiming (..), windowMatches)
import Marvel.Window qualified as W

data GameState = Unstarted | InProgress | Finished FinishedStatus
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON)

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
  deriving anyclass (ToJSON, FromJSON)

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

data Game = Game
  { gamePhase :: Phase
  , gameState :: GameState
  , -- players in player order
    gamePlayerOrder :: [IdentityId]
  , gameEntities :: Entities
  , gameBoostEntities :: Entities
  , gameRemovedEntities :: Entities
  , gameQuestion :: HashMap IdentityId Question
  , gameUsedAbilities :: HashMap IdentityId [(Ability, Int)]
  , gameActivePlayer :: IdentityId
  , gameYou :: Maybe IdentityId
  -- ^ who "you" is while a player's card, choice or window is looked at: its
  -- controller, or the player choosing or being offered it (see 'asYou'); otherwise
  -- the active player
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
  -- ^ the players as they sat down, which never changes: the server finds whose seat is
  -- whose by it, while the player order turns as the first player passes
  , gamePlayedThisRound :: [(IdentityId, CardDef)]
  -- ^ the cards each player has played this round, for "max 1 per round" and "the first
  -- ally played each round"
  , gamePlayedThisPhase :: [(IdentityId, CardDef)]
  , -- | the cards played since the current player's turn began
    gamePlayedThisTurn :: [(IdentityId, CardDef)]
  , -- | each treachery revealed this phase from the encounter deck, and for whom, in order
    gameTreacheriesRevealedThisPhase :: [(IdentityId, TreacheryId)]
  -- ^ and this phase, for "max 1 per phase"
  , gameAreas :: [Area]
  -- ^ the game areas, the first the one every game starts with ("Marvel.Area")
  , gameSignatureSchemes :: HashMap VillainId SideSchemeId
  -- ^ each villain's own side scheme, as Wrecker's Day of Reckoning
  , gameAtEndOfPhase :: [Message]
  -- ^ what is to happen "at the end of the phase", in the order it was set to
  , gameAllAreas :: Bool
  -- ^ whether what is looked at is looked at in every area, as whether a card matches
  -- is, wherever it is ('inAnyArea')
  , gameCampaign :: Maybe Campaign
  -- ^ the campaign this game is a scenario of, as it was dealt and as the game writes in
  -- its log ("Marvel.Campaign")
  , gameVictoryDisplay :: [EncounterCard]
  -- ^ the victory display: the out-of-play area shared by all players that cards with
  -- victory X go to as they are defeated (RR Victory Display), the first added first
  , gamePeril :: Maybe IdentityId
  -- ^ the player resolving a card with peril: the others cannot help them (RR Peril)
  , gameCollection :: [Card]
  -- ^ The Collection (Infiltrate the Museum): an out-of-play area shared by all players,
  -- player and encounter cards faceup in it, the first added first
  , gameCollecting :: [CardId]
  -- ^ the cards in play on their way into The Collection, leaving play for it
  }
  deriving stock (Show, Eq, Generic)

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
  toJSON = genericToJSON $ aesonOptions $ Just "game"

instance FromJSON Game where
  parseJSON = withObject "Game" $ \o ->
    -- Saves from before phase limits were tracked have no phase history.
    genericParseJSON (aesonOptions $ Just "game") $
      Object
        ( KeyMap.insertWith (\_ existing -> existing) "playedThisPhase" (toJSON ([] :: [Value]))
            . KeyMap.insertWith (\_ existing -> existing) "treacheriesRevealedThisPhase" (toJSON ([] :: [Value]))
            . KeyMap.insertWith (\_ existing -> existing) "playedThisTurn" (toJSON ([] :: [Value]))
            . KeyMap.insertWith (\_ existing -> existing) "victoryDisplay" (toJSON ([] :: [Value]))
            . KeyMap.insertWith (\_ existing -> existing) "collection" (toJSON ([] :: [Value]))
            . KeyMap.insertWith (\_ existing -> existing) "collecting" (toJSON ([] :: [Value]))
            $ o
        )

diff :: Game -> Game -> Diff.Patch
diff a b = Diff.diff (toJSON a) (toJSON b)

patch :: Game -> Diff.Patch -> Result Game
patch g p = case Diff.patch p (toJSON g) of
  Error e -> Error e
  Success a -> fromJSON a

newtype GameAppT a = GameAppT {unGameAppT :: ReaderT GameApp IO a}
  deriving newtype (MonadReader GameApp, Functor, Applicative, Monad, MonadFail, MonadIO, MonadCatch, MonadThrow)

-- | A game being played: the game and its queue, the random generator it rolls with
-- (stored with the game, so a reloaded game rolls the same), and where what it says to
-- its clients goes.
data GameApp = GameApp
  { appGame :: IORef Game
  , appQueue :: IORef [Message]
  , appDebugLogger :: Maybe DebugLogger
  , appRandomGen :: IORef StdGen
  , appFrontend :: FrontendMessage -> IO ()
  }

instance MonadRandom GameAppT where
  getRandomR lohi = withGen (randomR lohi)
  getRandom = withGen random
  getRandomRs lohi = randomRs lohi <$> withGen splitGen
  getRandoms = randoms <$> withGen splitGen

withGen :: (StdGen -> (a, StdGen)) -> GameAppT a
withGen f = do
  ref <- asks appRandomGen
  liftIO $ atomicModifyIORef' ref (swap . f)

instance Frontend GameAppT where
  send msg = asks appFrontend >>= \f -> liftIO (f msg)

instance HasGame GameAppT where
  getGame = getGameRef >>= liftIO . readIORef

instance HasQueue GameAppT where
  getQueue = asks appQueue

instance HasDebugLogger GameApp where
  debugLogger = appDebugLogger

instance HasGameRef GameAppT where
  getGameRef = asks appGame

instance MonadGame GameApp GameAppT

newApp :: (MonadIO m) => Game -> [Message] -> m GameApp
newApp g msgs = newGameApp g msgs (mkStdGen 0) (const $ pure ())

-- | A game to play from its queue, rolling with the generator given and telling its
-- clients through the function given.
newGameApp :: (MonadIO m) => Game -> [Message] -> StdGen -> (FrontendMessage -> IO ()) -> m GameApp
newGameApp g msgs gen frontend = liftIO do
  gameRef <- newIORef g
  queueRef <- newIORef msgs
  genRef <- newIORef gen
  pure $ GameApp gameRef queueRef Nothing genRef frontend

-- | The game, its queue and its random generator as they now stand.
currentState :: GameApp -> IO (Game, [Message], StdGen)
currentState app = (,,) <$> readIORef (appGame app) <*> readIORef (appQueue app) <*> readIORef (appRandomGen app)

runGameApp :: (MonadIO m) => GameApp -> GameAppT a -> m a
runGameApp gameApp = liftIO . flip runReaderT gameApp . unGameAppT

-- | "Each player": the players in "your" game area, in turn order; at a table not
-- split into areas, every player (TOAFK insert p.7).
getPlayers :: (HasGame m) => m [IdentityId]
getPlayers = do
  you <- getYou
  getsGame \g -> case gameAreas g of
    _ : _ : _ | not (gameAllAreas g) ->
      let here = areaId <$> areaOfTarget g (IdentityRef you)
       in filter (\p -> (areaId <$> areaOfTarget g (IdentityRef p)) == here) (gamePlayerOrder g)
    _ -> gamePlayerOrder g

-- | Every player, in turn order, whatever area they are in.
getAllPlayers :: (HasGame m) => m [IdentityId]
getAllPlayers = getsGame gamePlayerOrder

getActivePlayerId :: (HasGame m) => m IdentityId
getActivePlayerId = toId <$> getActivePlayer

getPlayerCount :: (HasGame m) => m Int
getPlayerCount = length <$> getsGame gamePlayerOrder

-- | The players who started the scenario, the eliminated among them (RR Per Player Icon).
getStartingPlayerCount :: (HasGame m) => m Int
getStartingPlayerCount = getsGame \g -> max (length $ gameSeats g) (length $ gamePlayerOrder g)

-- | Each unique card in play, and each identity, as a title and the subtitles and
-- alter-ego titles that go with it: a unique card matching one cannot enter play.
-- | In "your" game area: a unique card in one area places no limitation on the others
-- (TOAFK insert p.7).
-- | The unique cards a unique player card would match (RR Unique Icon): the players'
-- cards in your area, and the unique minions and villains in play (the Nebula ally and the
-- Nebula minion match).
getUniqueTitlesInPlay :: (HasGame m) => m [(Text, [Text])]
getUniqueTitlesInPlay = do
  players <- map snd <$> (getUniqueCardsIn . areaId =<< getYourArea)
  encounter <- getsGame (map snd . uniqueEncounterCardsInPlay)
  pure $ players <> encounter

-- | The unique encounter cards in play: minions, villains and attachments.
uniqueEncounterCardsInPlay :: Game -> [(Target, (Text, [Text]))]
uniqueEncounterCardsInPlay g =
  [ (t, (title n, maybe [] pure (subtitle n)))
  | (t, def) <-
      [(MinionRef k, getCardDef m) | (k, m) <- HashMap.toList (gameMinions g)]
        <> [(VillainRef k, getCardDef v) | (k, v) <- HashMap.toList (gameVillains g)]
        <> [(AttachmentRef k, getCardDef a) | (k, a) <- HashMap.toList (gameAttachments g)]
  , cdUnique def
  , let n = cdName def
  ]

-- | Whether the unique card entering play (already made, as the target) matches another
-- card in play: the player's cards in their area, or a unique encounter card.
matchesUniqueInPlay :: (HasGame m) => IdentityId -> Target -> CardDef -> m Bool
matchesUniqueInPlay ident self def
  | not (cdUnique def) = pure False
  | otherwise = do
      players <- getUniqueCardsIn . areaId =<< getAreaOf ident
      encounter <- getsGame uniqueEncounterCardsInPlay
      let named = (title (cdName def), maybe [] pure (subtitle (cdName def)))
      pure $ any (uniqueMatch named . snd) [x | x@(t, _) <- players <> encounter, t /= self]

-- | The unique cards in play in the area, by title and its other names: the players'
-- allies, supports and upgrades, and their identities.
getUniqueCardsIn :: (HasGame m) => AreaId -> m [(Target, (Text, [Text]))]
getUniqueCardsIn aid = getsGame \g ->
  let
    split = case gameAreas g of
      _ : _ : _ -> True
      _ -> False
    here t = not split || (areaId <$> areaOfTarget g t) == Just aid
    named n = (title n, maybe [] pure (subtitle n))
    unique xs = [(t, named $ cdName def) | (t, def) <- xs, cdUnique def, here t]
    -- its hero title, and its alter-ego title as the other name: Ant-Man's two hero
    -- forms share the one title
    identity (k, p) =
      let sides = HashMap.elems $ playerIdentitySides $ toAttrs p
          heroes = nub [title (cdName $ getCardDef h) | HeroSide h <- sides]
          alterEgos = [title (cdName $ getCardDef a) | AlterEgoSide a <- sides]
       in case heroes of
            t : ts | here (IdentityRef k) -> [(IdentityRef k, (t, ts <> alterEgos))]
            _ -> []
   in
    unique [(AllyRef k, getCardDef a) | (k, a) <- HashMap.toList (gameAllies g)]
      <> unique [(SupportRef k, supportCardDef (toAttrs s)) | (k, s) <- HashMap.toList (gameSupports g)]
      <> unique [(UpgradeRef k, getCardDef u) | (k, u) <- HashMap.toList (gameUpgrades g)]
      <> concatMap identity (HashMap.toList $ gamePlayers g)

-- | Who "you" is: the player 'asYou' names, or else the active player.
getYou :: (HasGame m) => m IdentityId
getYou = getsGame \g -> fromMaybe (gameActivePlayer g) (gameYou g)

-- | Look at the game as the player given: "you" on a player card is its controller, or
-- the player who would play or trigger it (RR "You").
asYou :: (HasGame m) => IdentityId -> ReaderT Game m a -> m a
asYou ident body = getGame >>= runReaderT body . (youL ?~ ident)

-- * Game areas

getAreas :: (HasGame m) => m [Area]
getAreas = getsGame gameAreas

-- | The area a game element is in, by its ref: a minion is where the player it is
-- engaged with is, a player's card where that player is, and what no area lists is in
-- the first.
areaOfTarget :: Game -> Target -> Maybe Area
areaOfTarget g = \case
  IdentityRef i -> listing (elem i . areaPlayers)
  VillainRef v -> listing (elem v . areaVillains)
  EnemyRef (EnemyVillainId v) -> areaOfTarget g (VillainRef v)
  EnemyRef (EnemyMinionId m) -> areaOfTarget g (MinionRef m)
  CharacterRef (IdentityCharacter i) -> areaOfTarget g (IdentityRef i)
  CharacterRef (AllyCharacter a) -> areaOfTarget g (AllyRef a)
  CharacterRef (VillainCharacter v) -> areaOfTarget g (VillainRef v)
  CharacterRef (MinionCharacter m) -> areaOfTarget g (MinionRef m)
  MainSchemeRef s -> listing (elem s . areaMainSchemes)
  SideSchemeRef s -> listing (elem s . areaSideSchemes)
  SchemeRef (SchemeMainSchemeId s) -> areaOfTarget g (MainSchemeRef s)
  SchemeRef (SchemeSideSchemeId s) -> areaOfTarget g (SideSchemeRef s)
  EnvironmentRef e -> listing (elem e . areaEnvironments)
  MinionRef m -> player . getMinionEngagedIdentity =<< lookup m (gameMinions g)
  AllyRef a -> player . getAllyController =<< lookup a (gameAllies g)
  SupportRef s -> player . getSupportController =<< lookup s (gameSupports g)
  UpgradeRef u -> player . getUpgradeController =<< lookup u (gameUpgrades g)
  -- an obligation is in its player's play area, an attachment where what it is on is
  ObligationRef o -> player =<< Obligation.obligationOwner . toAttrs =<< lookup o (gameObligations g)
  AttachmentRef a -> lookup a (gameAttachments g) >>= \x ->
    let at = toAttrs x
     in headMay $
          catMaybes
            [ areaOfTarget g . EnemyRef =<< Attachment.attachmentEnemy at
            , player =<< Attachment.attachmentIdentity at
            , areaOfTarget g . SideSchemeRef =<< Attachment.attachmentSideScheme at
            ]
  _ -> headMay (gameAreas g)
 where
  listing p = areaListing p (gameAreas g)
  player = areaOfTarget g . IdentityRef

getAreaOfTarget :: (HasGame m, IsRef a) => a -> m AreaId
getAreaOfTarget a = getsGame \g -> maybe defaultAreaId areaId (areaOfTarget g (toTarget a))

-- | What of the map is in the area, by its key.
keysInArea :: (HasGame m) => (k -> Target) -> (Game -> HashMap k v) -> AreaId -> m [k]
keysInArea ref entities aid = getsGame \g -> [k | k <- HashMap.keys (entities g), (areaId <$> areaOfTarget g (ref k)) == Just aid]

-- | In the area, whoever's area "you" are in.
getMainSchemesIn :: (HasGame m) => AreaId -> m [MainSchemeId]
getMainSchemesIn = keysInArea MainSchemeRef gameMainSchemes

getMinionsIn :: (HasGame m) => AreaId -> m [MinionId]
getMinionsIn = keysInArea MinionRef gameMinions

getObligationsIn :: (HasGame m) => AreaId -> m [ObligationId]
getObligationsIn = keysInArea ObligationRef gameObligations

getSideSchemesIn :: (HasGame m) => AreaId -> m [SideSchemeId]
getSideSchemesIn = keysInArea SideSchemeRef gameSideSchemes

-- | The player's nemesis minions, out of play: set aside with their nemesis set, or in
-- the encounter deck or its discard pile. Each copy of a nemesis minion is one (Venom's
-- Enraged Symbiotes, rulesheet FAQ).
getNemesisMinionsOutOfPlay :: (HasGame m) => IdentityId -> m [EncounterCard]
getNemesisMinionsOutOfPlay ident = getsGame \g -> fromMaybe [] do
  p <- lookup ident (gamePlayers g)
  let
    scenario = toAttrs (gameScenario g)
    set = getNemesisSet (toCardCode p)
    cards = onlyEncounterCards (scenarioSetAsideCards scenario) <> scenarioEncounterDeck scenario <> scenarioDiscard scenario
  pure [c | c <- cards, cdEncounterSet (getCardDef c) == Just set, Minions.isNemesisMinion (getCardDef c)]

-- | Whether a patrol minion is engaged with the player: they cannot thwart the main
-- scheme with the cards they control, by a basic thwart or an ability labeled thwart (RR
-- Patrol, Thwart); removing threat otherwise is not a thwart.
isPatrolled :: (HasGame m) => IdentityId -> m Bool
isPatrolled ident = selectAny $ MinionEngagedWith (IdentityWithId ident) <> MinionWithKeyword Patrol

-- | The card code of the main scheme in play.
getMainSchemeCode :: (HasGame m) => MainSchemeId -> m (Maybe CardCode)
getMainSchemeCode mid = getsGame (fmap (toCardCode . getCardDef) . lookup mid . gameMainSchemes)

-- | The area the player is in.
getAreaOf :: (HasGame m) => IdentityId -> m Area
getAreaOf ident = getsGame \g -> fromMaybe (newArea defaultAreaId []) (areaOfTarget g (IdentityRef ident))

-- | The area "you" are in: what "the villain", "the main scheme" and "the encounter
-- deck" are there.
getYourArea :: (HasGame m) => m Area
getYourArea = getAreaOf =<< getYou

-- | Whether the game element is in "your" area. With one area everything is.
inYourArea :: (HasGame m) => Target -> m Bool
-- an environment is in every area (RR FAQ, The Once and Future Kang)
inYourArea (EnvironmentRef _) = pure True
inYourArea t = do
  you <- getYou
  getsGame \g -> case gameAreas g of
    _ : _ : _ | not (gameAllAreas g) -> (areaId <$> areaOfTarget g t) == (areaId <$> areaOfTarget g (IdentityRef you))
    _ -> True

-- | Looking in every area: whether a game element matches is so wherever it is, as only
-- what may be chosen or affected is "your" area's.
inAnyArea :: (HasGame m) => ReaderT Game m a -> m a
inAnyArea body = getGame >>= runReaderT body . (allAreasL .~ True)

-- | Only those in "your" area: players cannot target game elements in other areas.
inYourAreaOnly :: (HasGame m) => (a -> Target) -> [a] -> m [a]
inYourAreaOnly ref = filterM (inYourArea . ref)

-- | The villains in play in the area, as it lists them.
villainsIn :: Game -> AreaId -> [VillainId]
villainsIn g aid = case break ((== aid) . areaId) (gameAreas g) of
  (before, a : _) -> filter inPlay (areaVillains a) <> (if null before then unlisted else [])
  _ -> []
 where
  inPlay v = HashMap.member v (gameVillains g)
  unlisted = filter (`notElem` concatMap areaVillains (gameAreas g)) (HashMap.keys $ gameVillains g)

getVillainsInArea :: (HasGame m) => AreaId -> m [VillainId]
getVillainsInArea aid = getsGame (`villainsIn` aid)

-- | The area's active villain: the one it names while he is in play, or else the first
-- of its villains.
activeVillainIn :: Game -> AreaId -> Maybe VillainId
activeVillainIn g aid = case [a | a <- gameAreas g, areaId a == aid] of
  a : _ | Just v <- areaActiveVillain a, HashMap.member v (gameVillains g) -> Just v
  _ -> headMay (villainsIn g aid)

getActiveVillainIn :: (HasGame m) => AreaId -> m (Maybe VillainId)
getActiveVillainIn aid = getsGame (`activeVillainIn` aid)

-- | What "the encounter deck" is for the player: their area's active villain's own deck,
-- when he has one, or else the encounter deck.
getDeckKeyFor :: (HasGame m) => IdentityId -> m DeckKey
getDeckKeyFor ident = do
  aid <- areaId <$> getAreaOf ident
  getsGame \g -> case activeVillainIn g aid of
    Just v | HashMap.member (VillainDeckKey v) (scenarioDecks $ toAttrs $ gameScenario g) -> VillainDeckKey v
    _ -> EncounterDeckKey

-- | The counters on the main scheme: Zola's test counters, say.
getMainSchemeCounters :: (HasGame m) => MainSchemeId -> m Natural
getMainSchemeCounters ms = getsGame (maybe 0 (mainSchemeCounters . toAttrs) . lookup ms . gameMainSchemes)

-- | The player cards set aside out of play, as an encounter set's Captive allies.
getSetAsidePlayerCards :: (HasGame m) => m [PlayerCard]
getSetAsidePlayerCards = getsGame \g -> [c | PlayerCard c <- scenarioSetAsideCards (toAttrs $ gameScenario g)]

-- | The deck and its discard pile, each top card first.
-- | Every encounter deck in the game: the encounter deck, then any others.
getDeckKeys :: (HasGame m) => m [DeckKey]
getDeckKeys = getsGame $ deckKeys . toAttrs . gameScenario

-- | The encounter cards set aside, out of play.
getSetAsideEncounterCards :: (HasGame m) => m [EncounterCard]
getSetAsideEncounterCards = getsGame $ onlyEncounterCards . scenarioSetAsideCards . toAttrs . gameScenario

getPile :: (HasGame m) => DeckKey -> m Pile
getPile = \case
  EncounterDeckKey -> getsGame \g -> let a = toAttrs (gameScenario g) in Pile (scenarioEncounterDeck a) (scenarioDiscard a)
  key -> getsGame $ fromMaybe emptyPile . lookup key . scenarioDecks . toAttrs . gameScenario

-- | The pure step: the game with the area changed.
overArea :: AreaId -> (Area -> Area) -> Game -> Game
overArea aid f = areasL %~ map \a -> if areaId a == aid then f a else a

-- | The main scheme enters play in the area: its starting threat, then its "When
-- Revealed".
addMainSchemeIn :: (ReverseQueue m, MonadThrow m) => AreaId -> CardCode -> Game -> m Game
addMainSchemeIn aid cardCode g = do
  mainSchemeId <- getRandom
  case lookupMainScheme cardCode mainSchemeId of
    Just x -> do
      mainSchemeStep mainSchemeId MainSchemePlaceInitialThreat
      mainSchemeStep mainSchemeId RevealMainScheme
      pure $ g & (entitiesL . mainSchemesL . at mainSchemeId ?~ x) & overArea aid (\a -> a {areaMainSchemes = areaMainSchemes a <> [mainSchemeId]})
    Nothing -> throwM $ MissingCardCode "AddMainScheme" cardCode

-- | The game element is put into the area the player is in.
intoAreaOf :: IdentityId -> (Area -> Area) -> Game -> Game
intoAreaOf ident f g = maybe g (\a -> overArea (areaId a) f g) (areaOfTarget g (IdentityRef ident))

-- | The player the villain's activation under way is against: who resolves its boost
-- cards, the "you" of their boost abilities.
getVillainActivationPlayer :: (HasGame m) => VillainId -> m (Maybe IdentityId)
getVillainActivationPlayer vid = getsGame \g -> Villain.villainActivationPlayer . toAttrs =<< lookup vid (gameVillains g)

-- | The villain's attack under way, if one is.
getVillainAttack :: (HasGame m) => VillainId -> m (Maybe Attack)
getVillainAttack vid = getsGame \g -> Villain.villainAttackDetails =<< lookup vid (gameVillains g)

-- | The code the scenario is set up by.
getScenarioCode :: (HasGame m) => m CardCode
getScenarioCode = getsGame (toId . gameScenario)

-- | The villain's ATK, as modified.
getVillainAttackValue :: (HasGame m) => VillainId -> m Natural
getVillainAttackValue vid = getsGame (lookup vid . gameVillains) >>= maybe (pure 0) (Villain.getModifiedAttack . toAttrs)

-- | How much of an attack's damage would go past the character's remaining hit points:
-- the hero's defense taken off first, none past a tough status card.
getExcessDamage :: (HasGame m) => CharacterId -> Natural -> m Natural
getExcessDamage character damage = case character of
  IdentityCharacter ident ->
    getsGame (lookup ident . gamePlayers) >>= \case
      Nothing -> pure 0
      Just p -> do
        hp <- getModifiedHp p
        let a = toAttrs p
            dealt = subtractNatural a.playerIdentityDamageReduction damage
            remaining = subtractNatural a.playerIdentityDamage hp
        pure $ if a.playerIdentityTough && dealt > 0 then 0 else subtractNatural remaining dealt
  AllyCharacter aid ->
    getsGame (lookup aid . gameAllies) >>= \case
      Nothing -> pure 0
      Just ally -> do
        hp <- Ally.getModifiedHitPoints (toAttrs ally)
        let remaining = subtractNatural (getAllyDamage ally) hp
        pure $ if (toAttrs ally).allyTough then 0 else subtractNatural remaining damage
  _ -> pure 0
-- | Every trait of each environment in play.
getEnvironmentTraits :: (HasGame m) => m (HashSet Trait)
getEnvironmentTraits = getsGame $ foldMap (cdTraits . getCardDef) . HashMap.elems . gameEnvironments

-- | The counters on "your" main scheme, as None Shall Pass's delay counters.
getYourMainSchemeCounters :: (HasGame m) => m Natural
getYourMainSchemeCounters = do
  schemes <- filterM (inYourArea . MainSchemeRef . toId) . HashMap.elems =<< getsGame gameMainSchemes
  pure $ sum $ map (mainSchemeCounters . toAttrs) schemes


-- | How many times a "When Revealed" the player reveals resolves: once, and once more
-- for each ability that says so (Media Coverage).
whenRevealedTimes :: (HasGame m) => IdentityId -> m Int
whenRevealedTimes ident = (+ 1) . fromIntegral . count (== ResolveWhenRevealedAgain) <$> getModifiers ident

-- | The player a boost card turned up for the enemy's activation is resolved by: the
-- one the villain's activation is against, or the player whose turn it is.
getBoostedPlayer :: (HasGame m) => EnemyId -> m IdentityId
getBoostedPlayer = \case
  EnemyVillainId vid -> maybe getActivePlayerId pure =<< getVillainActivationPlayer vid
  EnemyMinionId _ -> getActivePlayerId

-- | The obligations in the player's play area.
countObligationsOf :: (HasGame m) => IdentityId -> m Natural
countObligationsOf ident =
  getsGame $ fromIntegral . length . filter ((== Just ident) . Obligation.obligationOwner . toAttrs) . HashMap.elems . gameObligations

-- | The villain's stage.
getVillainStage :: (HasGame m) => VillainId -> m Natural
getVillainStage vid = getsGame $ maybe 0 (Villain.villainStage . toAttrs) . lookup vid . gameVillains

-- | The enemy's printed hit points: a villain's for the number of players.
getEnemyPrintedHitPoints :: (HasGame m) => EnemyId -> m Natural
getEnemyPrintedHitPoints = \case
  EnemyVillainId vid ->
    getsGame (lookup vid . gameVillains) >>= \case
      Nothing -> pure 0
      Just v -> fromIntegral <$> fromGameValue (unHp $ Villain.villainStartingHp $ toAttrs v)
  EnemyMinionId mid -> getsGame $ maybe 0 (unHp . getMinionPrintedHitPoints) . lookup mid . gameMinions

-- | The counters on the environment, main scheme or attachment.
getCountersOn :: (HasGame m, IsRef a) => a -> m Natural
getCountersOn a = getsGame \g -> case toTarget a of
  EnvironmentRef eid -> maybe 0 (field EnvironmentCounters) (lookup eid $ gameEnvironments g)
  MainSchemeRef mid -> maybe 0 (field MainSchemeCounters) (lookup mid $ gameMainSchemes g)
  AttachmentRef aid -> maybe 0 (field AttachmentCounters) (lookup aid $ gameAttachments g)
  _ -> 0

-- | How many cards in play have the trait: of every type, each identity one card.
countCardsInPlayWithTrait :: forall m. (HasGame m) => Trait -> m Natural
countCardsInPlayWithTrait t = do
  g <- getGame
  let printed :: [CardDef] -> Natural
      printed = count (member t . cdTraits)
      defs :: (HasCardDef a) => EntityMap a -> [CardDef]
      defs = map getCardDef . HashMap.elems
      traited :: (HasTraits a) => EntityMap a -> m Natural
      traited = fmap (count (member t)) . traverse getTraits . HashMap.elems
  characters <- sum <$> sequence [traited (gamePlayers g), traited (gameAllies g), traited (gameMinions g)]
  pure $
    characters
      + printed (map (supportCardDef . toAttrs) $ HashMap.elems $ gameSupports g)
      + printed
        ( defs (gameVillains g)
            <> defs (gameUpgrades g)
            <> defs (gameAttachments g)
            <> defs (gameTreacheries g)
            <> defs (gameObligations g)
            <> defs (gameSideSchemes g)
            <> defs (gameMainSchemes g)
            <> defs (gameEnvironments g)
        )

getActivePlayer :: (HasGame m) => m PlayerIdentity
getActivePlayer = getsGame $ \g ->
  case lookup (g ^. activePlayerL) (g ^. entitiesL . playersL) of
    Just p -> p
    Nothing -> error "Missing player"

runPreGameMessage :: (HasGame m, HasQueue m, MonadThrow m, MonadRandom m) => Message -> Game -> m Game
runPreGameMessage msg g = runQueueT $ case msg of
  -- begun before the scenario takes the card from where it was, so where that was is
  -- known; a card still to be revealed for a player eliminated meanwhile is discarded
  RevealEncounterCard ident card | not (HashMap.member ident (gamePlayers g)) -> do
    discardCard card
    pure g
  RevealEncounterCard ident card -> do
    let
      scenario = toAttrs (gameScenario g)
      source
        | card `elem` (scenarioDiscard scenario <> concatMap pileDiscard (HashMap.elems $ scenarioDecks scenario)) = W.RevealedFromDiscardPile
        | EncounterCard card `elem` scenarioSetAsideCards scenario = W.RevealedFromSetAside
        | otherwise = W.RevealedFromEncounterDeck
    focusing [EncounterCard card] $
      checkWindows [W.Window When $ W.EncounterCardRevealed source ident card]
    revealEncounterCardWithoutWindows ident card
    pure g
  CheckWindows windows -> do
    endCheckWindows
    pure $ g & windowDepthL +~ 1 & windowsL %~ (windows :)
  -- We want to empty the queue for triggering a resolution
  EndCheckWindows ->
    pure $
      g
        & (windowDepthL -~ 1)
        & (windowsL %~ drop 1)
        & ( usedAbilitiesL
              . each
              %~ filter
                ( \(a, d) ->
                    (baseLimit (abilityLimit a) /= PerWindow 1) || d < gameWindowDepth g
                )
          )
  _ -> pure g

-- | A side scheme that cannot leave play (The Sleeper Awakened) stays where it is.
whenCanLeavePlay :: (HasGame m) => Game -> SideSchemeId -> m Game -> m Game
whenCanLeavePlay g sid body = do
  stays <- elem CannotLeavePlay <$> getModifiers (SideSchemeRef sid)
  if stays then pure g else body

-- | A card leaving play for its discard pile: one bound for The Collection goes there
-- instead, and otherwise "when a card would be placed into a discard pile from play"
-- is answered first (Collector).
discardFromPlay :: (ReverseQueue m, IsCard card) => Game -> card -> m ()
discardFromPlay g c
  | toCardId card `elem` gameCollecting g = placeInCollection card
  | otherwise = do
      checkWindows [W.Window W.Would $ W.WouldBeDiscardedFromPlay either']
      discardCard card
 where
  card = toCard c
  either' = case card of
    PlayerCard pc -> Left pc
    EncounterCard ec -> Right ec

-- | The card a game element in play was made from, by its ref.
cardInPlay :: Game -> Target -> Maybe Card
cardInPlay g = \case
  AllyRef k -> toCard <$> lookup k (gameAllies g)
  SupportRef k -> toCard . toAttrs <$> lookup k (gameSupports g)
  UpgradeRef k -> toCard <$> lookup k (gameUpgrades g)
  MinionRef k -> toCard <$> lookup k (gameMinions g)
  AttachmentRef k -> toCard <$> lookup k (gameAttachments g)
  SideSchemeRef k -> toCard <$> lookup k (gameSideSchemes g)
  EnvironmentRef k -> toCard <$> lookup k (gameEnvironments g)
  ObligationRef k -> toCard <$> lookup k (gameObligations g)
  _ -> Nothing

-- | The card in play, by its ref: the entity made from it.
inPlayRef :: Game -> Card -> Maybe Target
inPlayRef g card =
  headMay $
    catMaybes
      [ AllyRef k <$ lookup k (gameAllies g)
      , SupportRef k' <$ lookup k' (gameSupports g)
      , UpgradeRef k'' <$ lookup k'' (gameUpgrades g)
      , MinionRef m <$ lookup m (gameMinions g)
      , AttachmentRef a <$ lookup a (gameAttachments g)
      , SideSchemeRef s' <$ lookup s' (gameSideSchemes g)
      , EnvironmentRef e <$ lookup e (gameEnvironments g)
      , ObligationRef o <$ lookup o (gameObligations g)
      ]
 where
  u = unCardId (toCardId card)
  k = AllyId u
  k' = SupportId u
  k'' = UpgradeId u
  m = MinionId u
  a = AttachmentId u
  s' = SideSchemeId u
  e = EnvironmentId u
  o = ObligationId u

runGameMessage :: (HasGame m, HasQueue m, MonadThrow m, MonadRandom m) => Message -> Game -> m Game
runGameMessage msg g@Game {..} = runQueueT $ case msg of
  RecordInCampaignLog r -> pure $ g & campaignL %~ fmap (Campaign.recordInLog r)
  -- a campaign game won is first written in the log by the scenario's Victory steps
  GameOver Won | Just c <- gameCampaign, Campaign.currentScenario c == Just (toId gameScenario) -> do
    clearQueue
    campaignStep CampaignVictory
    gameOver Won
    pure g
  -- an expert campaign's last scenario lost: Red Skull conquers the world
  GameOver Lost
    | Just c <- gameCampaign
    , Campaign.isExpertCampaign c
    , Campaign.scenarioNumber c == length c.scenarios -> do
        clearQueue
        pure $ g & stateL .~ Finished Lost & campaignL .~ Just c {Campaign.lost = True}
  PutObligationIntoPlay ident card -> do
    let obligation = lookupObligation (toCardCode card) (ObligationId $ unCardId $ pcCardId card)
    -- it is the player's, in their play area: told as an obligation revealed is
    obligationStep (toId obligation) (RevealObligation ident)
    pure $ g & entitiesL . obligationsL %~ insertMap (toId obligation) obligation
  -- setup, as the rules reference's appendix II has it: a first player, the identities'
  -- obligations and nemesis sets set aside, the scenario, then everyone's hand
  StartGame -> do
    when (null gamePlayerOrder) $ throwM NoPlayers
    for_ gamePlayerOrder (`identityStep` SetupIdentity)
    -- the choices the last victory left are cards for the decks, so first
    when (isJust gameCampaign) $ campaignStep CampaignChoices
    for_ gamePlayerOrder (`identityStep` PutSetupCardsIntoPlay)
    startScenario
    when (isJust gameCampaign) $ campaignStep CampaignSetup
    setupPlayers
    case gamePlayerOrder of
      players@(p : _ : _) ->
        chooseOnePrompted p "$label.chooseFirstPlayer" $
          [ targeting first (setPlayerOrder (after <> before))
          | (n, first) <- zip [0 ..] players
          , let (before, after) = splitAt n players
          ]
      _ -> pure ()
    -- what the Campaign Instructions do with the starting hands
    when (isJust gameCampaign) $ campaignStep (CampaignSetupThen "startingHands")
    pure $ g {gameState = InProgress}
  SetupPlayers -> do
    for_ gamePlayerOrder \p -> do
      setActivePlayer p
      identityStep p DealStartingHand
    for_ (take 1 gamePlayerOrder) setActivePlayer
    beginPhase PlayerPhase
    pure g
  SetPlayerOrder xs -> pure $ g {gamePlayerOrder = xs, gameActivePlayer = fromMaybe gameActivePlayer (headMay xs)}
  -- a player eliminated after their turn was queued (damage at the end of a phase) is
  -- passed over
  SetActivePlayer ident
    | HashMap.member ident (gamePlayers g) -> pure $ g & activePlayerL .~ ident
    | otherwise -> pure g
  BeginPhase phase ->
    pure $ g & phaseL .~ phase & playedThisPhaseL .~ [] & treacheriesRevealedThisPhaseL .~ [] & usedAbilitiesL . each %~ filter (not . perPhase . baseLimit . abilityLimit . fst)
  AtEndOfPhase msgs -> pure $ g & atEndOfPhaseL <>~ msgs
  -- what was to happen at the end of the phase happens first, as the phase ends
  EndPhase _ -> do
    pushAll gameAtEndOfPhase
    pure $ g & atEndOfPhaseL .~ []
  -- the first player token goes to the next player, who takes the first turn of the round
  PassFirstPlayer -> do
    let order = case gamePlayerOrder of
          [] -> []
          (p : ps) -> ps <> [p]
    pure $ g & playerOrderL .~ order & activePlayerL .~ fromMaybe gameActivePlayer (headMay order)
  -- faceup into The Collection: a card in play leaves play for it, by way of
  -- 'RemoveFromPlay' ('discardFromPlay' sends it on); a card out of play is taken from
  -- where it is by what holds it (a deck, a hand, a discard pile)
  PlaceInCollection card -> case inPlayRef g card of
    Just ref -> do
      removeFromPlay ref
      pure $ g & collectingL %~ (toCardId card :)
    Nothing ->
      pure $
        g
          & (collectingL %~ filter (/= toCardId card))
          & (collectionL %~ (<> [card]) . filter ((/= toCardId card) . toCardId))
  -- a card leaving The Collection for its owner's discard pile
  DiscardedCard card -> pure $ g & collectionL %~ filter ((/= toCardId card) . toCardId)
  -- out of play for the victory display, not discarded: what is attached to it is
  -- discarded as it would be (RR Victory X)
  AddToVictoryDisplay target -> case target of
    MinionRef mid -> case lookup mid (gameMinions g) of
      Nothing -> pure g
      Just minion -> do
        let a = toAttrs minion
        for_ (minionUpgrades a) removeFromPlay
        for_ (minionAttachments a) removeFromPlay
        minionDisengages (minionEngagedIdentity a) mid
        pure $
          g
            & (entitiesL . minionsL %~ deleteMap mid)
            & (removedEntitiesL . minionsL %~ insertMap mid minion)
            & (victoryDisplayL <>~ [ec | EncounterCard ec <- [toCard minion]])
    SideSchemeRef sid -> case lookup sid (gameSideSchemes g) of
      Nothing -> pure g
      Just scheme -> whenCanLeavePlay g sid do
        for_ [uid | (uid, u) <- HashMap.toList (gameUpgrades g), upgradeAttachedSideScheme (toAttrs u) == Just sid] removeFromPlay
        for_ [aid | (aid, at) <- HashMap.toList (gameAttachments g), attachmentSideScheme (toAttrs at) == Just sid] removeFromPlay
        pure $
          g
            & (entitiesL . sideSchemesL %~ deleteMap sid)
            & (removedEntitiesL . sideSchemesL %~ insertMap sid scheme)
            & (areasL . each %~ \area -> area {areaSideSchemes = filter (/= sid) (areaSideSchemes area)})
            & (victoryDisplayL <>~ [ec | EncounterCard ec <- [toCard scheme]])
    AttachmentRef aid -> case lookup aid (gameAttachments g) of
      Nothing -> pure g
      Just at -> do
        attachmentRemoved aid
        pure $ g & (entitiesL . attachmentsL %~ deleteMap aid) & (victoryDisplayL <>~ [ec | EncounterCard ec <- [toCard at]])
    _ -> error $ "Cannot add to the victory display: " <> show target
  DefeatedLeavesPlay target -> do
    let def = case target of
          MinionRef k -> getCardDef <$> lookup k (gameMinions g)
          SideSchemeRef k -> getCardDef <$> lookup k (gameSideSchemes g)
          AttachmentRef k -> getCardDef <$> lookup k (gameAttachments g)
          _ -> Nothing
        victory = maybe False (any isVictory . cdKeywords) def
        isVictory = \case
          Victory _ -> True
          _ -> False
    push $ if victory then AddToVictoryDisplay target else RemoveFromPlay target
    pure g
  SetPeril who -> pure $ g & perilL .~ who
  -- each card's icons and one more for each amplify icon in play, counted as
  -- 'CountBoostIcons' counts them
  CountActivationBoostIcons target cards -> do
    amplify <- getAmplifyCount
    counts <- for cards \card -> (card,) . (+ amplify) <$> getBoostIcons card
    for_ cards \card -> checkWindows [W.Window When $ W.CountingBoostIcons card]
    push $ WithBoostIcons target (toJSON ()) counts
    pure g
  RemoveFromGame target -> case target of
    ObligationRef oid -> pure $ g & (entitiesL . obligationsL %~ deleteMap oid)
    -- a villain out of the game leaves play as he would, what is on him discarded
    VillainRef _ -> g <$ push (RemoveFromPlay target)
    -- a main scheme out of the game, what is attached to it discarded, leaves its area
    -- without one until a stage takes its place
    MainSchemeRef mid -> do
      for_ [uid | (uid, u) <- HashMap.toList (gameUpgrades g), upgradeAttachedMainScheme (toAttrs u) == Just mid] removeFromPlay
      pure $ g & (entitiesL . mainSchemesL %~ deleteMap mid) & (areasL . each %~ \a -> a {areaMainSchemes = filter (/= mid) (areaMainSchemes a)})
    -- out of the game, not discarded: what is attached to it is discarded
    SideSchemeRef sid -> whenCanLeavePlay g sid do
      for_
        [ uid
        | (uid, u) <- HashMap.toList (gameUpgrades g)
        , upgradeAttachedSideScheme (toAttrs u) == Just sid
        ]
        removeFromPlay
      for_ [aid | (aid, a) <- HashMap.toList (gameAttachments g), attachmentSideScheme (toAttrs a) == Just sid] removeFromPlay
      pure $ g & (entitiesL . sideSchemesL %~ deleteMap sid) & (areasL . each %~ \a -> a {areaSideSchemes = filter (/= sid) (areaSideSchemes a)})
    MinionRef mid -> case lookup mid (gameMinions g) of
      Nothing -> pure g
      Just minion -> do
        let a = toAttrs minion
        for_ (minionUpgrades a) removeFromPlay
        for_ (minionAttachments a) removeFromPlay
        minionDisengages (minionEngagedIdentity a) mid
        pure $ g & (entitiesL . minionsL %~ deleteMap mid)
    AttachmentRef aid -> do
      when (HashMap.member aid (gameAttachments g)) $ attachmentRemoved aid
      pure $ g & (entitiesL . attachmentsL %~ deleteMap aid)
    CardIdRef _ -> pure g -- Should be handled by Identity
    -- an ally in play taken out of the game (tucked): what is attached to it discarded
    AllyRef aid -> do
      for_ (lookup aid $ gameAllies g) \ally -> do
        for_ (allyUpgrades $ toAttrs ally) removeFromPlay
        identityStep (getAllyController ally) (AllyRemoved aid)
      pure $ g & (entitiesL . alliesL %~ deleteMap aid)
    -- an upgrade out of the game, not discarded: the cards it holds are
    UpgradeRef uid -> do
      for_ (lookup uid $ gameUpgrades g) \upgrade -> do
        upgradeRemoved uid
        for_ (upgradeHeldCards $ toAttrs upgrade) discardCard
      pure $ g & (entitiesL . upgradesL %~ deleteMap uid)
    _ -> error "Unhandled"
  RemoveFromPlay target -> case target of
    IdentityRef iid -> do
      when (length (g ^. entitiesL . playersL) <= 1) (gameOver Lost)
      -- the last player of an area defeated, with players left in others
      area <- getAreaOf iid
      when (length (g ^. entitiesL . playersL) > 1 && all (== iid) (areaPlayers area)) $
        push (AreaDefeated $ areaId area)
      let order = filter (/= iid) gamePlayerOrder
          -- whoever is left takes over from an eliminated active player
          next = case break (== iid) gamePlayerOrder of
            (before, _ : after) -> headMay (after <> before)
            _ -> headMay order
      pure $
        g
          & (entitiesL . playersL %~ deleteMap iid)
          & (playerOrderL .~ order)
          & (activePlayerL %~ \active -> if active == iid then fromMaybe active next else active)
          & (areasL . each %~ \a -> a {areaPlayers = filter (/= iid) (areaPlayers a)})
    -- a villain leaving play takes his own deck and side scheme with him, and what is
    -- attached to him is discarded; the next villain of his area becomes active
    VillainRef vid -> case lookup vid (gameVillains g) of
      Nothing -> pure g
      Just v -> do
        let a = toAttrs v
        for_ (Villain.villainAttachments a) (removeFromPlay . AttachmentRef)
        for_ (villainUpgrades a) (removeFromPlay . UpgradeRef)
        push $ RemoveDeck (VillainDeckKey vid)
        let signature = lookup vid gameSignatureSchemes
        -- what is attached to his side scheme leaves play with it
        for_
          [ aid
          | (aid, att) <- HashMap.toList (gameAttachments g)
          , isJust signature
          , attachmentSideScheme (toAttrs att) == signature
          ]
          (removeFromPlay . AttachmentRef)
        let
          leave area =
            -- he stays listed in his area, out of play there: his kept deck is shown in it
            let rest = filter (/= vid) (areaVillains area)
             in area
                  { areaActiveVillain = if areaActiveVillain area == Just vid then headMay rest else areaActiveVillain area
                  , areaSideSchemes = filter ((/= signature) . Just) (areaSideSchemes area)
                  }
        pure $
          g
            & (entitiesL . villainsL %~ deleteMap vid)
            & (removedEntitiesL . villainsL %~ insertMap vid v)
            & (entitiesL . sideSchemesL %~ maybe id deleteMap signature)
            & (signatureSchemesL %~ deleteMap vid)
            & (areasL . each %~ leave)
    AllyRef aid -> do
      for_ (lookup aid $ gameAllies g) $ \ally -> do
        let ident = getAllyController ally
        -- what is attached to it is discarded with it
        for_ (allyUpgrades $ toAttrs ally) removeFromPlay
        identityStep ident $ AllyRemoved aid
        discardFromPlay g ally
      -- kept as removed, so an ability it was discarded to pay for still resolves
      pure $
        g
          & (entitiesL . alliesL %~ deleteMap aid)
          & (removedEntitiesL . alliesL %~ maybe id (insertMap aid) (lookup aid $ gameAllies g))
    -- a permanent support does not leave play: the Milano stays for the next first player
    SupportRef sid | maybe False (elem Permanent . cdKeywords . supportCardDef . toAttrs) (lookup sid $ gameSupports g) -> pure g
    SupportRef sid -> do
      -- it is discarded, and the cards attached to it facedown with it
      for_ (lookup sid $ gameSupports g) $ \support -> do
        identityStep (getSupportController support) (SupportRemoved sid)
        discardFromPlay g (toAttrs support)
        for_ (supportHeldCards $ toAttrs support) discardCard
      -- kept as removed, so an ability it was discarded to pay for still resolves
      pure $
        g
          & (entitiesL . supportsL %~ deleteMap sid)
          & (removedEntitiesL . supportsL %~ maybe id (insertMap sid) (lookup sid $ gameSupports g))
    UpgradeRef uid -> do
      case (lookup uid $ gameUpgrades g) of
        Nothing -> pure g
        -- the permanent keyword: it cannot leave play ("Permanent")
        Just upgrade | isPermanent upgrade -> pure g
        Just upgrade -> do
          upgradeRemoved uid
          discardFromPlay g upgrade
          for_ (upgradeHeldCards $ toAttrs upgrade) discardCard
          pure $
            g
              & (entitiesL . upgradesL %~ deleteMap uid)
              & (removedEntitiesL . upgradesL %~ insertMap uid upgrade)
    -- a permanent attachment does not leave play: it attaches anew ('Marvel.Attachment.Types')
    AttachmentRef aid | maybe False isPermanent (lookup aid $ gameAttachments g) -> pure g
    AttachmentRef aid -> do
      for_ (lookup aid $ gameAttachments g) $ \attachment -> do
        attachmentRemoved aid
        discardFromPlay g attachment
      pure $ g & (entitiesL . attachmentsL %~ deleteMap aid)
    -- discarded, its own cards with it, and no longer engaged
    MinionRef mid -> case lookup mid (gameMinions g) of
      Nothing -> pure g
      Just minion -> do
        let a = toAttrs minion
        for_ (minionUpgrades a) removeFromPlay
        for_ (minionAttachments a) removeFromPlay
        minionDisengages (minionEngagedIdentity a) mid
        discardFromPlay g minion
        pure $
          g
            & (entitiesL . minionsL %~ deleteMap mid)
            & (removedEntitiesL . minionsL %~ insertMap mid minion)
    TreacheryRef tid -> do
      for_ (lookup tid $ gameTreacheries g) discardCard
      pure $ g & (entitiesL . treacheriesL %~ deleteMap tid)
    ObligationRef oid -> do
      for_ (lookup oid $ gameObligations g) (discardFromPlay g)
      pure $ g & (entitiesL . obligationsL %~ deleteMap oid)
    -- a villain's own side scheme cannot leave play while he is in play
    SideSchemeRef sid | sid `elem` HashMap.elems gameSignatureSchemes -> pure g
    SideSchemeRef sid -> whenCanLeavePlay g sid do
      -- what is attached to it is discarded with it, first
      for_
        [ uid
        | (uid, u) <- HashMap.toList (gameUpgrades g)
        , upgradeAttachedSideScheme (toAttrs u) == Just sid
        ]
        removeFromPlay
      for_ (lookup sid $ gameSideSchemes g) (discardFromPlay g)
      pure $
        g
          & (entitiesL . sideSchemesL %~ deleteMap sid)
          & (removedEntitiesL . sideSchemesL %~ maybe id (insertMap sid) (lookup sid $ gameSideSchemes g))
    EnvironmentRef eid -> do
      for_ (lookup eid $ gameEnvironments g) (discardFromPlay g)
      pure $ g & (entitiesL . environmentsL %~ deleteMap eid)
    _ -> error "Unhandled target"
  AddMainScheme cardCode -> do
    you <- getYou
    addMainSchemeIn (maybe defaultAreaId areaId $ areaOfTarget g (IdentityRef you)) cardCode g
  AddSetupMainScheme cardCode -> do
    mainSchemeId <- getRandom
    case lookupMainScheme cardCode mainSchemeId of
      Just x -> do
        setupMainScheme
        mainSchemeStep mainSchemeId MainSchemePlaceInitialThreat
        mainSchemeStep mainSchemeId RevealMainScheme
        for_ (HashMap.keys (gameVillains g)) \villainId ->
          checkWindows [W.Window When $ W.RevealVillain villainId W.RevealedFromVillain]
        you <- getYou
        pure $ g & (entitiesL . mainSchemesL . at mainSchemeId ?~ x) & intoAreaOf you (\a -> a {areaMainSchemes = areaMainSchemes a <> [mainSchemeId]})
      Nothing -> throwM $ MissingCardCode "AddSetupMainScheme" cardCode
  -- the main scheme leaves the game, each card attached to it discarded, and the next
  -- takes its place in its area
  ReplaceMainScheme old cardCode -> do
    for_
      [ uid
      | (uid, u) <- HashMap.toList (gameUpgrades g)
      , upgradeAttachedMainScheme (toAttrs u) == Just old
      ]
      removeFromPlay
    let aid = maybe defaultAreaId areaId $ areaOfTarget g (MainSchemeRef old)
    addMainSchemeIn aid cardCode $
      g
        & (entitiesL . mainSchemesL %~ deleteMap old)
        & (areasL . each %~ \a -> a {areaMainSchemes = filter (/= old) (areaMainSchemes a)})
  AddVillain cardCode -> do
    villainId <- getRandom
    case lookupVillain cardCode villainId of
      Just x -> do
        villainStep villainId SetVillainHp
        you <- getYou
        let join a = a {areaVillains = areaVillains a <> [villainId], areaActiveVillain = areaActiveVillain a <|> Just villainId}
        pure $ g & (entitiesL . villainsL . at villainId ?~ x) & intoAreaOf you join
      Nothing -> throwM $ MissingCardCode "AddVillain" cardCode
  UsedAbility ident a ->
    pure $ g & usedAbilitiesL %~ HashMap.insertWith (<>) ident [(a, gameWindowDepth)]
  DisableActiveCost ident -> pure $ g & activeCostL %~ filter ((/= ident) . activeCostId)
  SetActiveCost activeCost -> do
    createdActiveCost (activeCostId activeCost)
    pure $ g & activeCostL %~ (activeCost :)
  CreatedEffect def source matcher -> do
    effectId <- getRandom
    ident <- toId <$> getActivePlayer
    let effect = createEffect effectId (toCardCode def) source matcher
    effectStep effectId (UsedEffect ident)
    pure $ g & (entitiesL . effectsL %~ insertMap effectId effect)
  DisabledEffect effectId ->
    pure $ g & (entitiesL . effectsL %~ deleteMap effectId)
  PutCardIntoPlay ident card payment mWindow wasPlayed -> do
    case cdCardType (getCardDef card) of
      AllyType -> do
        let ally = createAlly ident card
        allyStep (toId ally) (AllyOwnedBy $ fromMaybe ident $ pcOwner card)
        allyStep (toId ally) (AllyPaidWith payment)
        when (Toughness `member` cdKeywords (getCardDef card)) $ push $ GiveStatus (AllyRef $ toId ally) ToughStatus
        identityStep ident (AllyCreated $ toId ally)
        checkWindows $
          W.Window After (W.AllyEnteredPlay $ toId ally)
            : [W.Window After $ W.PlayedAlly (toId ally) | wasPlayed]
        -- back in play, no longer the copy kept as it was discarded
        pure $ g & (entitiesL . alliesL %~ insertMap (toId ally) ally) & (removedEntitiesL . alliesL %~ deleteMap (toId ally))
      SupportType -> do
        -- played under the control of the player it was played on, if any; it stays its
        -- owner's, whose discard pile it goes to (RR Owner)
        let controller = case paymentTarget payment of
              Just (IdentityRef p) -> p
              _ -> ident
            support = createSupport controller card
        supportStep (toId support) (SupportOwnedBy $ fromMaybe ident $ pcOwner card)
        identityStep controller (SupportCreated $ toId support)
        when wasPlayed $ checkWindows [W.Window After $ W.PlayedSupport (toId support)]
        pure $ g & (entitiesL . supportsL %~ insertMap (toId support) support)
      UpgradeType -> do
        let upgrade = createUpgrade ident card
        upgradeStep (toId upgrade) (UpgradeOwnedBy $ fromMaybe ident $ pcOwner card)
        upgradeStep (toId upgrade) (PlayedUpgrade $ paymentTarget payment)
        pure $ g & (entitiesL . upgradesL %~ insertMap (toId upgrade) upgrade)
      EventType -> do
        let
          event = createEvent ident card
          -- "when you play" answers once the cost is paid, before the event resolves;
          -- "after you play", once it has
          played timing = checkWindows [W.Window timing $ W.EventPlayed ident (toId event) (getCardDef card)]
        played When
        resolveEventEffect ident (toId event) (PlayedEvent ident payment (W.windowType <$> mWindow))
        eventStep (toId event) ResolvedEvent
        played After
        pure $ g & (entitiesL . eventsL %~ insertMap (toId event) event)
      _ -> error "Unhandled"
  -- the effect of an attack or thwart event is its player's attack or thwart: a stun or
  -- confusion replaces it as it would resolve, its cost paid ("Stunned", "Confused")
  ResolveEventEffect ident (EventMessage eid playEvent) -> do
    let labels = maybe [] (cdAbilitySubTypes . eventCardDef . toAttrs) (lookup eid (gameEvents g))
    stunned <- identityMatches StunnedIdentity ident
    confused <- identityMatches ConfusedIdentity ident
    let stops = Ability.Attack `elem` labels && stunned
        baffles = Ability.Thwart `elem` labels && confused
    -- a status that cancels any of its labels cancels the whole of it, and each such
    -- status is removed (RR Labeled Ability)
    if stops || baffles
      then do
        when stops $ removeStun ident
        when baffles $ removeConfused ident
      else do
        -- a defense-labeled ability makes its player's identity the defender of the enemy
        -- attack it is used during, if there is none yet: not a basic defense, so no DEF
        -- (RR "Defend, Defense")
        when (Ability.Defense `elem` labels) do
          attacker <- selectOne (AttackingEnemy <> UndefendedEnemy)
          for_ attacker \enemy -> enemyDefendedBy enemy (IdentityCharacter ident)
        eventStep eid playEvent
    pure g
  ResolveEventWithoutPlaying ident card -> do
    let event = createEvent ident card
    eventStep (toId event) (PlayedEvent ident NoPayment Nothing)
    eventStep (toId event) ResolvedEvent
    pure $ g & (entitiesL . eventsL %~ insertMap (toId event) event)
  PutBoostIntoPlay target ident -> case target of
    MinionRef minionId ->
      case g ^. boostEntitiesL . minionsL . at minionId of
        Just minion -> do
          engageMinion minionId ident
          pure $
            g
              & (boostEntitiesL . minionsL %~ deleteMap (toId minion))
              & (entitiesL . minionsL %~ insertMap (toId minion) minion)
              & (boostCardsL %~ deleteMap target)
        _ -> error "messed up somewhere"
    -- any other card ("put Goblin Nation into play") enters play as it would revealed,
    -- without its windows, and is no longer a boost card to discard
    _ -> case lookup target gameBoostCards of
      Just card -> do
        putEncounterCardIntoPlay ident card
        pure $ g & boostCardsL %~ deleteMap target
      Nothing -> pure g
  -- "Boost: reveal this card": it is no longer a boost card, and is revealed for the
  -- player
  RevealBoost target ident -> case lookup target gameBoostCards of
    Just card -> do
      revealEncounterCard ident card
      pure $
        g
          & (boostCardsL %~ deleteMap target)
          & (boostEntitiesL . sideSchemesL %~ deleteMap (SideSchemeId $ unCardId $ ecCardId card))
          & (boostEntitiesL . attachmentsL %~ deleteMap (AttachmentId $ unCardId $ ecCardId card))
          & (boostEntitiesL . treacheriesL %~ deleteMap (TreacheryId $ unCardId $ ecCardId card))
          & (boostEntitiesL . minionsL %~ deleteMap (MinionId $ unCardId $ ecCardId card))
    Nothing -> pure g
  RevealBoostCard card enemyId -> do
    player <- getActivePlayerId
    let
      -- after it is turned faceup, before its boost ability resolves
      turnedFaceup target = do
        checkWindows [W.Window After $ W.FlippedBoostCard card enemyId]
        revealedAsBoost target enemyId
    case cdCardType (getCardDef card) of
      TreacheryType -> do
        let treachery = createTreachery card
        turnedFaceup (toTarget treachery)
        pure $
          g
            & (boostEntitiesL . treacheriesL %~ insertMap (toId treachery) treachery)
            & (boostCardsL . at (toTarget treachery) ?~ card)
      MinionType -> do
        let minion = createMinion player card
        turnedFaceup (toTarget minion)
        pure $
          g
            & (boostEntitiesL . minionsL %~ insertMap (toId minion) minion)
            & (boostCardsL . at (toTarget minion) ?~ card)
      AttachmentType -> do
        let attachment = createAttachment card
        turnedFaceup (toTarget attachment)
        pure $
          g
            & ( boostEntitiesL
                  . attachmentsL
                  %~ insertMap (toId attachment) attachment
              )
            & (boostCardsL . at (toTarget attachment) ?~ card)
      ObligationType -> do
        let obligation = createObligation card
        turnedFaceup (toTarget obligation)
        pure $
          g
            & ( boostEntitiesL
                  . obligationsL
                  %~ insertMap (toId obligation) obligation
              )
            & (boostCardsL . at (toTarget obligation) ?~ card)
      SideSchemeType -> do
        let sideScheme = createSideScheme card
        turnedFaceup (toTarget sideScheme)
        pure $
          g
            & ( boostEntitiesL
                  . sideSchemesL
                  %~ insertMap (toId sideScheme) sideScheme
              )
            & (boostCardsL . at (toTarget sideScheme) ?~ card)
      EnvironmentType -> do
        let environment = createEnvironment card
        turnedFaceup (toTarget environment)
        pure $
          g
            & (boostEntitiesL . environmentsL %~ insertMap (toId environment) environment)
            & (boostCardsL . at (toTarget environment) ?~ card)
      _ -> pure g
  CountBoostIcons target value cards -> do
    counts <- for cards \card -> (card,) <$> getBoostIcons card
    for_ cards \card -> checkWindows [W.Window When $ W.CountingBoostIcons card]
    push $ WithBoostIcons target value counts
    pure g
  ClearBoosts -> do
    for_ (HashMap.elems gameBoostCards) discardCard
    pure $ g & boostEntitiesL .~ defaultEntities & boostCardsL .~ mempty
  ClearRemoved -> pure $ g & removedEntitiesL .~ defaultEntities
  -- indirect damage (RR): assigned a point at a time among the characters the player
  -- controls that can take damage, none past what would defeat it (a tough one too, its
  -- card preventing what it is given), then dealt at once; what cannot be assigned is
  -- ignored
  AssignIndirectDamage ident source damage assigned ->
    g <$ assignIndirectDamage ident [ident] source damage assigned (indirectDamageAssigned ident source)
  -- among the players: the group's, the first player assigning it
  AssignIndirectDamageAmongPlayers source n assigned -> do
    first <- selectJust FirstPlayer
    players <- getPlayers
    g <$ assignIndirectDamage first players source (toDamage n FromAbility) assigned \d as -> push (AssignIndirectDamageAmongPlayers source (damageAmount d) as)
  -- a card set aside from play (Escape the Museum's Milano) leaves it, out of play and
  -- not discarded: permanent or not; the scenario keeps it set aside
  SetAside cards -> do
    let ids = map toCardId cards
        gone = [s | s <- HashMap.elems (gameSupports g), CardId (unSupportId (toId s)) `elem` ids]
    for_ gone \s -> identityStep (getSupportController s) (SupportRemoved (toId s))
    pure $ g & entitiesL . supportsL %~ \m -> foldr (deleteMap . toId) m gone
  -- divided damage (Giant Wasp, Wasp Sting): a point at a time among the enemies
  -- matching, then each share dealt at once; an attack's attacks each enemy dealt one
  AssignDividedDamage ident source damage matcher assigned -> do
    enemies <- selectList (DamageableEnemy <> matcher)
    let n = damageAmount damage
        assign e k = push $ AssignDividedDamage ident source damage {damageAmount = n - k} matcher ((e, k) : assigned)
    case enemies of
      _ | n == 0 || null enemies ->
        pushAll . simultaneousDamage
          =<< traverse
            (\(e, k) -> capture $ resolveAs ident $ DamageEnemy (enemyRef e) source damage {damageAmount = k})
            (HashMap.toList $ HashMap.fromListWith (+) assigned)
      [e] -> assign e n
      _ -> chooseOnePrompted ident "$label.assignDividedDamage" [targeting (enemyRef e) (assign e 1) | e <- enemies]
    pure g
  -- divided threat removal: a point at a time among the schemes matching, none past a
  -- scheme's threat, then each scheme given a share thwarted (Wasp rulesheet)
  AssignDividedThreat ident source basic upTo n matcher assigned -> do
    let given s = sum [k | (s', k) <- assigned, s' == s]
        labeledThwart = maybe False (elem Ability.Thwart . cdAbilitySubTypes) (abilityCardDef g (toTarget source))
    patrolled <- isPatrolled ident
    -- a thwart, the main scheme not while a minion with patrol is engaged (RR Patrol)
    let thwartable = \case
          SchemeMainSchemeId _ -> not (patrolled && (basic || labeledThwart))
          _ -> True
    schemes <- filter thwartable <$> selectList matcher
    open <- forMaybeM schemes \s -> do
      threat <- selectCount SchemeThreat (SchemeWithId s)
      let room = subtractNatural (given s) threat
      pure $ guard (room > 0) $> (s, room)
    let assign s k = push $ AssignDividedThreat ident source basic upTo (n - k) matcher ((s, k) : assigned)
        done = AssignDividedThreat ident source basic upTo 0 matcher assigned
    case open of
      _ | n == 0 || null open ->
        for_ (HashMap.toList $ HashMap.fromListWith (+) assigned) \(s, k) ->
          resolveAs ident $ (if basic then BasicThwartScheme else ThwartScheme) (schemeRef s) source k
      [(s, room)] | not upTo -> assign s (min n room)
      _ ->
        chooseOnePrompted ident "$label.assignDividedThreat" $
          [targeting (schemeRef s) (assign s 1) | (s, _) <- open]
            <> [labeled "$label.done" (push done) | upTo]
    pure g
  RevealedEncounterCard ident card -> do
    -- each "When Revealed" resolves once, and once more for each ability that says so
    -- (Media Coverage); incite and surge are "When Revealed" abilities (RR)
    times <- whenRevealedTimes ident
    let whenRevealed = replicateM_ times
    -- a card with a "When Revealed" waits for the player to resolve it, so each player
    -- sees what is happening; then incite, its threat first, then the rest of the card (a
    -- treachery's after the window as it is revealed, which may cancel it)
    let revealing t = do
          when (hasWhenRevealed $ toCardCode card) $ askToResolveWhenRevealed ident t
          whenRevealed (incite card)
    -- surge is resolved after the rest of the card; treacheries surge as they resolve
    let surges = when (Marvel.Keyword.Surge `member` cdKeywords (getCardDef card)) (whenRevealed $ surge ident)
    case cdCardType (getCardDef card) of
      AttachmentType -> do
        let attachment = createAttachment card
        rest <- capture do
          revealing (toTarget attachment)
          attachmentStep (toId attachment) (RevealAttachment ident)
          surges
        push $ EntersPlayUnlessUnique ident True (toTarget attachment) rest
        pure $
          g
            & (entitiesL . attachmentsL %~ insertMap (toId attachment) attachment)
      MinionType -> do
        let
          minion = createMinion ident card
          isTough = Toughness `member` cdKeywords (getCardDef card)
        -- "when it would enter play" (the Nebula minion) comes before a matching unique
        -- card keeps it out
        focusing [toCard minion] $ checkWindows [W.Window When $ W.MinionEnteredPlay $ toId minion]
        rest <- capture do
          revealing (toTarget minion)
          when isTough $ enemyBecomesTough (toId minion)
          focusing [toCard minion] $ whenRevealed $ minionStep (toId minion) (RevealMinion ident)
          engageMinion (toId minion) ident
          checkWindows [W.Window After $ W.MinionEnteredPlay $ toId minion]
          surges
        push $ EntersPlayUnlessUnique ident True (toTarget minion) rest
        pure $ g & (entitiesL . minionsL %~ insertMap (toId minion) minion)
      TreacheryType -> do
        let treachery = createTreachery card
            peril = Peril `member` cdKeywords (getCardDef card)
        -- with peril, no one else may help while the player resolves it (RR Peril)
        when peril $ push (SetPeril (Just ident))
        -- TODO: FOCUS CARDS SHOULD BE CARDS...
        focusing [toCard treachery] do
          checkWindows [W.Window When $ W.RevealTreachery (toId treachery) W.RevealedFromEncounterDeck]
          askToResolveWhenRevealed ident treachery
          whenRevealed $ treacheryStep (toId treachery) TreacheryIncite
          whenRevealed $ treacheryStep (toId treachery) $ RevealTreachery ident
          treacheryStep (toId treachery) $ CheckTreacheryCondition ident
        treacheryStep (toId treachery) $ ResolvedTreachery ident
        when peril $ push (SetPeril Nothing)
        pure $
          g
            & (entitiesL . treacheriesL %~ insertMap (toId treachery) treachery)
            & treacheriesRevealedThisPhaseL <>~ [(ident, toId treachery)]
      ObligationType -> do
        let
          obligation = createObligation card
          -- "Give to the Peter Parker player": the player whose identity it comes with
          -- resolves it; with them gone it is removed from the game and another card is
          -- revealed. ponytail: every obligation so far is given to its identity's player
          owners
            -- one of an encounter set is the revealing player's (RR, Obligation)
            | isJust (cdEncounterSet (getCardDef card)) = [ident]
            | otherwise =
                [ toId p | p <- HashMap.elems (gamePlayers g), getCardDef card `elem` identityObligations p
                ]
        case owners of
          -- "ignore the card's ability, remove it from the game, and reveal an
          -- additional encounter card": not surge
          [] -> do
            drawAndRevealEncounterCard ident
            pure g
          owner : _ -> do
            whenRevealed (incite card)
            -- TODO: FOCUS CARDS SHOULD BE CARDS...
            focusing [toCard obligation] $
              obligationStep (toId obligation) $ RevealObligation owner
            obligationStep (toId obligation) $ ResolvedObligation owner
            pure $
              g
                & (entitiesL . obligationsL %~ insertMap (toId obligation) obligation)
      SideSchemeType -> do
        let sideScheme = createSideScheme card
        revealing (toTarget sideScheme)
        sideSchemeStep (toId sideScheme) SideSchemePlaceInitialThreat
        whenRevealed $ sideSchemeStep (toId sideScheme) RevealSideScheme
        surges
        pure $
          g
            & (entitiesL . sideSchemesL %~ insertMap (toId sideScheme) sideScheme)
            & intoAreaOf ident (\a -> a {areaSideSchemes = areaSideSchemes a <> [toId sideScheme]})
      EnvironmentType -> do
        let environment = createEnvironment card
        revealing (toTarget environment)
        environmentStep (toId environment) EnvironmentEntered
        surges
        pure $ g & environmentIntoPlay ident environment
      _ -> error "Unhandled"
  AttachEncounterCard card enemy -> do
    let attachment = createAttachment card
    attachTo (toId attachment) enemy
    pure $ g & (entitiesL . attachmentsL %~ insertMap (toId attachment) attachment) & (boostCardsL %~ HashMap.filter (/= card))
  -- a boost card shuffled into the encounter deck is not discarded with the others
  ShuffleIntoEncounterDeck cards -> pure $ g & boostCardsL %~ HashMap.filter (`notElem` cards)
  PutEncounterCardIntoPlay ident card -> case cdCardType (getCardDef card) of
    MinionType -> do
      let
        minion = createMinion ident card
        isTough = Toughness `member` cdKeywords (getCardDef card)
      checkWindows [W.Window When $ W.MinionEnteredPlay $ toId minion]
      rest <- capture do
        when isTough $ enemyBecomesTough (toId minion)
        engageMinion (toId minion) ident
        checkWindows [W.Window After $ W.MinionEnteredPlay $ toId minion]
      push $ EntersPlayUnlessUnique ident False (toTarget minion) rest
      pure $ g & (entitiesL . minionsL %~ insertMap (toId minion) minion)
    -- put into play, not revealed: no surge
    EnvironmentType -> do
      let environment = createEnvironment card
      environmentStep (toId environment) EnvironmentEntered
      pure $ g & environmentIntoPlay ident environment
    -- an encounter support, as the Milano, is put into play under the player's control,
    -- its card owned by no player
    SupportType -> do
      g <$ push (PutCardIntoPlay ident (MkPlayerCard (ecCardId card) (ecCardDef card) Nothing (Just ident)) NoPayment Nothing False)
    -- attached as its own "attach to" says, not revealed: no "When Revealed" and no surge
    AttachmentType -> do
      let attachment = createAttachment card
      rest <- capture $ attachmentStep (toId attachment) (RevealAttachment ident)
      push $ EntersPlayUnlessUnique ident False (toTarget attachment) rest
      pure $ g & (entitiesL . attachmentsL %~ insertMap (toId attachment) attachment)
    _ -> do
      revealEncounterCardWithoutWindows ident card
      pure g
  PutTopOfDeckIntoPlayAsDrone ident -> case lookup ident (gamePlayers g) of
    Nothing -> pure g
    Just player -> do
      let
        attrs = toAttrs player
        reshuffle = do
          identityStep ident ShuffleIdentityDiscardBackIntoDeck
          dealEncounterCard ident
      case unDeck (playerIdentityDeck attrs) of
        []
          | null (unDiscard $ playerIdentityDiscard attrs) -> pure g
          | otherwise -> do
              reshuffle
              putTopOfDeckIntoPlayAsDrone ident
              pure g
        card : rest -> do
          let
            minionId = MinionId $ unCardId $ pcCardId card
            drone =
              overMinion (facedownCardL ?~ card) $
                lookupMinion "xdrone" ident minionId
          removeFromGame (CardIdRef $ pcCardId card)
          engageMinion minionId ident
          checkWindows [W.Window After $ W.MinionEnteredPlay minionId]
          when (null rest) reshuffle
          pure $ g & (entitiesL . minionsL %~ insertMap minionId drone)
  EndCheckWindows -> pure g
  EntersPlayUnlessUnique ident revealed target rest -> do
    let def = case target of
          MinionRef k -> getCardDef <$> lookup k (gameMinions g)
          AttachmentRef k -> getCardDef <$> lookup k (gameAttachments g)
          _ -> Nothing
    matched <- maybe (pure False) (matchesUniqueInPlay ident target) def
    if matched
      then do
        push $ KeptOutByUnique target
        removeFromPlay target
        when revealed $ dealEncounterCard ident
      else pushAll rest
    pure g
  KeptOutByUnique _ -> pure g
  IdentityEndedTurn ident ->
    pure $
      g
        & usedAbilitiesL . ix ident
          %~ filter
            ((/= PerTurn 1) . baseLimit . abilityLimit . fst)
  IdentityMessage ident (PlayedCard card _) ->
    pure $
      g
        & playedThisRoundL <>~ [(ident, getCardDef card)]
        & playedThisPhaseL <>~ [(ident, getCardDef card)]
        & playedThisTurnL <>~ [(ident, getCardDef card)]
  IdentityMessage _ BeginTurn -> pure $ g & playedThisTurnL .~ []
  EndRound ->
    pure $
      g
        & roundL +~ 1
        & playedThisRoundL .~ []
        & usedAbilitiesL . each
          %~ filter
            ((/= PerRound 1) . baseLimit . abilityLimit . fst)
  GameOver status -> do
    clearQueue
    pure $ g & stateL .~ Finished status
  ReturnToHand target -> case target of
    -- to its owner's hand
    AllyRef aid -> do
      for_ (lookup aid $ gameAllies g) $ \ally -> do
        let controller = getAllyController ally
            owner = allyOwner (toAttrs ally)
        identityStep controller (AllyRemoved aid)
        addToHand owner $ MkPlayerCard (CardId $ unAllyId aid) (getCardDef ally) (Just owner) (Just owner)
      pure $ g & entitiesL . alliesL %~ deleteMap aid
    -- to its owner's hand
    UpgradeRef uid | maybe False isPermanent (lookup uid $ gameUpgrades g) -> pure g
    UpgradeRef uid -> do
      for_ (lookup uid $ gameUpgrades g) $ \upgrade -> do
        let owner = upgradeOwner $ toAttrs upgrade
        upgradeRemoved uid
        for_ (upgradeHeldCards $ toAttrs upgrade) discardCard
        addToHand owner $ MkPlayerCard (CardId $ unUpgradeId uid) (getCardDef upgrade) (Just owner) (Just owner)
      pure $ g & entitiesL . upgradesL %~ deleteMap uid
    -- a played event goes back from its controller's discard pile, if it is still there
    EventRef eid -> do
      for_ (lookup eid $ gameEvents g) \event -> do
        let controller = eventController (toAttrs event)
        for_ [c | PlayerCard c <- [toCard (toAttrs event)]] \c -> do
          let inDiscard = maybe False (elem c . unDiscard . playerIdentityDiscard . toAttrs) (lookup controller $ gamePlayers g)
          when inDiscard $ addToHand controller c
      pure g
    _ -> error "unhandled"
  PendingActivations [(_, msgs)] -> g <$ pushAll msgs
  PendingActivations pending -> do
    first <- selectJust FirstPlayer
    chooseOneAtATimePrompted first "$label.chooseActivationOrder" [targeting v (pushAll msgs) | (v, msgs) <- pending]
    pure g
  -- the token goes where a redirect on the scheme sends it (The Master of Time), and
  -- stays in that scheme's area
  PlaceAccelerationToken ms -> do
    mods <- getModifiers (MainSchemeRef ms)
    let ms' = fromMaybe ms (lastMay [to | RedirectAccelerationTokens to <- mods, to /= ms])
        aid = maybe defaultAreaId areaId $ areaOfTarget g (MainSchemeRef ms')
    pure $ overArea aid (\a -> a {areaAccelerationTokens = areaAccelerationTokens a + 1}) g
  CreateArea aid players -> pure $ g & areasL %~ (movePlayers players aid . (<> [newArea aid []]))
  -- players come together: copies of a unique card are discarded down to one
  MovePlayerToArea ident aid -> do
    push $ DiscardDuplicateUniques aid
    pure $ g & areasL %~ movePlayers [ident] aid
  CombineAreas from into -> do
    push $ DiscardDuplicateUniques into
    pure $ g & areasL %~ combine from into
  -- "When players combine game areas, they must discard copies of unique cards until
  -- only one of each remains in that game area. If the players cannot agree which one to
  -- discard, the first player decides." (TOAFK insert p.7) An identity is never
  -- discarded: a card sharing its name goes.
  DiscardDuplicateUniques aid -> do
    uniques <- getUniqueCardsIn aid
    first <- selectJust FirstPlayer
    let
      groups [] = []
      groups ((t, n) : rest) =
        let (same, other) = partition (uniqueMatch n . snd) rest
         in ((t, n) : same) : groups other
      isIdentity = \case
        IdentityRef _ -> True
        _ -> False
    for_ [map fst grp | grp@(_ : _ : _) <- groups uniques] \copies ->
      case filter isIdentity copies of
        _ : _ -> for_ (filter (not . isIdentity) copies) discardControlled
        [] -> chooseOnePrompted first "$label.keepUniqueCopy" [targeting kept (for_ (filter (/= kept) copies) discardControlled) | kept <- copies]
    pure g
  SetActiveVillain vid -> do
    aid <- getAreaOfTarget vid
    pure $ g & overArea aid \a -> a {areaActiveVillain = Just vid}
  SetSignatureScheme vid sid -> pure $ g & signatureSchemesL %~ insertMap vid sid
  FocusCards cards -> pure $ g & focusedCardsL .~ cards
  UnfocusCards -> pure $ g & focusedCardsL .~ []
  ShowCardsTo ident cards -> pure $ g & privateCardsL . at ident ?~ cards
  HideCardsFrom ident -> pure $ g & privateCardsL . at ident .~ Nothing
  -- everyone may respond, the active player first, then the others in player order;
  -- forced abilities resolve before anyone is asked
  CheckWindows windows -> do
    active <- getActivePlayerId
    let order = active : filter (/= active) gamePlayerOrder
    checkWindowsFor order windows
    pure g
  CheckWindowsFor [] _ -> pure g
  -- a player eliminated while the window goes round is no longer asked
  CheckWindowsFor (ident : rest) windows | not (HashMap.member ident (gamePlayers g)) -> do
    checkWindowsFor rest windows
    pure g
  CheckWindowsFor (ident : rest) windows -> do
    abilities <- getAbilitiesFor ident
    attrs <- getIdentity ident
    -- a treachery's reveal, or an encounter card's, is cancelled by taking it off the
    -- queue: its window then closes, so a second cancel is not offered
    -- likewise damage prevented in full: nothing more answers it
    let open = \case
          W.Window _ (W.RevealTreachery tid _) -> isQueued \case
            TreacheryMessage tid' (RevealTreachery _) -> tid' == tid
            _ -> False
          W.Window _ (W.EncounterCardRevealed _ _ card) -> isQueued \case
            RevealedEncounterCard _ card' -> card' == card
            _ -> False
          W.Window _ (W.IdentityTakeDamage iid _) -> isQueued \case
            IdentityMessage iid' (IdentityDamaged _ _) -> iid' == iid
            _ -> False
          _ -> pure True
    openWindows <- filterM open windows
    -- "you" on what is offered is the player it is offered to; while another player
    -- resolves a card with peril, no card is played (RR Peril)
    let perilled = maybe False (/= ident) gamePeril
    playableCards <-
      if perilled then pure [] else asYou ident $ concatMapM
        (\w -> map (,Just w) <$> getWindowPlayableCards w attrs)
        openWindows
    validAbilities <-
      asYou ident $ filterM
        ( andM
            . sequence
              [ withinUseLimit ident
              , passesCriteria ident
              , passesCanAffordCost ident
              , \a -> anyM (`abilityInWindow` a) openWindows
              ]
        )
        abilities
    -- after a response the window is open again to everyone
    again <- Run <$> capture (checkWindows windows)
    let forcedAbiltiies = filter isForcedAbility validAbilities
        respond a = pure $ UseAbility (a & choicesL <>~ [again])
    case (forcedAbiltiies, validAbilities, playableCards) of
      ([], [], []) -> checkWindowsFor rest windows
      ([], as, cs) ->
        chooseOnePrompted ident "$label.respond" $
          labeled "$label.noResponse" (checkWindowsFor rest windows)
            : map respond as
              <> [pure $ TargetLabel (toRef c) [PlayCard c mwindow, again] | (c, mwindow) <- cs]
      (forced, _, _) -> chooseOnePrompted ident "$label.resolveForced" $ map respond forced
    pure g
  DeclareDefense ident enemyId defensePriority -> do
    declareDefenseWithout ident enemyId defensePriority []
    pure g
  -- each player decides for themselves whether a character they control defends (RR
  -- "Defend"): the attacked player first, who may ask another player before deciding;
  -- once they decline, each other player in turn is asked
  -- "you must defend with an ally you control, if able" (Melter, Ramming Speed): the
  -- attacked player chooses one of their allies able to defend, and may not decline, nor
  -- may another player defend instead; with none able, the attack is defended as any other
  DeclareDefenseWithout ident enemyId AllyIfAble [] -> do
    own <- selectList (UnexhaustedAlly <> AllyControlledBy (IdentityWithId ident))
    if null own
      then declareDefenseWithout ident enemyId AnyDefense []
      else chooseOnePromptedAbout ident "$label.declareDefender" enemyId [pure (AllyDefend a enemyId) | a <- own]
    pure g
  DeclareDefenseWithout ident enemyId defensePriority declined -> do
    allies <- selectList UnexhaustedAlly
    identities <- select $ UnexhaustedIdentity <> HeroIdentity
    controllers <- for allies \a -> (a,) <$> getsGame (fmap getAllyController . lookup a . gameAllies)
    let
      defensesOf owner =
        [ if owner == ident
          then pure (Defend enemyId)
          else targeting owner $ identityStep owner (SideMessage (Defended enemyId))
        | owner `member` identities
        ]
          <> [pure (AllyDefend a enemyId) | (a, Just controller) <- controllers, controller == owner]
      decline owner = declareDefenseWithout ident enemyId defensePriority (owner : declined)
      ask owner =
        chooseOnePromptedAbout owner "$label.askedToDefend" enemyId $
          defensesOf owner <> [labeled "$label.declineToDefend" (decline owner)]
      (before, from) = break (== ident) gamePlayerOrder
      others = [o | o <- from <> before, o /= ident, o `notElem` declined, notNull (defensesOf o)]
    if ident `elem` declined
      then for_ (headMay others) ask
      else
        chooseOnePromptedAbout ident "$label.declareDefender" enemyId $
          labeled "$label.noDefender" (decline ident)
            : defensesOf ident
              <> [targeting o (ask o) | o <- others]
    pure g
  _ -> pure g

perPhase :: Limit -> Bool
perPhase = \case
  PerPhase _ -> True
  _ -> False

getWindowPlayableCards ::
  ( HasCallStack
  , MonadThrow m
  , MonadRandom m
  , HasQueue m
  , HasGame m
  , Projection m PlayerIdentity
  ) =>
  Window ->
  PlayerIdentity ->
  m [PlayerCard]
getWindowPlayableCards window player =
  filterM
    (isWindowPlayable window player)
    cards
 where
  cards = unHand $ playerIdentityHand $ toAttrs player

isWindowPlayable ::
  ( HasCallStack
  , MonadThrow m
  , MonadRandom m
  , HasQueue m
  , HasGame m
  , Projection m PlayerIdentity
  ) =>
  Window ->
  PlayerIdentity ->
  PlayerCard ->
  m Bool
isWindowPlayable window attrs c = do
  affordable <- notNull <$> playableTargets attrs c
  passedCriteria <- passesCardCriteria ident c (cdCriteria def <> toAdditionalCriteria def)
  passedWindow <-
    maybe
      (pure False)
      (\matcher -> windowMatches matcher window GameRef)
      (cdResponseWindow def)
  passedPlayLimits <- checkPlayLimits attrs c
  pure $ affordable && passedCriteria && passedWindow && passedPlayLimits
 where
  ident = toId attrs
  def = getCardDef c

abilityInWindow :: (HasGame m) => Window -> Ability -> m Bool
abilityInWindow window a =
  maybe
    (pure False)
    (\matcher -> windowMatches matcher window source)
    (abilityWindow a)
 where
  source = abilitySource a

instance RunMessage Entities where
  runMessage msg entities =
    traverseOf (playersL . each) (\p -> asYou (toId p) $ runMessage msg p) entities
      >>= traverseOf (alliesL . each) (\a -> asYou (getAllyController a) $ runMessage msg a)
      >>= traverseOf (supportsL . each) (\a -> asYou (getSupportController a) $ runMessage msg a)
      >>= traverseOf (upgradesL . each) (\a -> asYou (getUpgradeController a) $ runMessage msg a)
      >>= traverseOf (treacheriesL . each) (runMessage msg)
      >>= traverseOf (obligationsL . each) (runMessage msg)
      >>= traverseOf (villainsL . each) (runMessage msg)
      >>= traverseOf (eventsL . each) (\a -> asYou (eventController $ toAttrs a) $ runMessage msg a)
      >>= traverseOf (effectsL . each) (runMessage msg)
      >>= traverseOf (attachmentsL . each) (runMessage msg)
      >>= traverseOf (minionsL . each) (runMessage msg)
      >>= traverseOf (sideSchemesL . each) (runMessage msg)
      >>= traverseOf (mainSchemesL . each) (runMessage msg)
      >>= traverseOf (environmentsL . each) (runMessage msg)

instance RunMessage Game where
  -- the last first, so what the first leads to is queued first
  runMessage (Simultaneously msgs) g = foldlM (flip runMessage) g (reverse msgs)
  runMessage msg g =
    runPreGameMessage msg g
      >>= traverseOf scenarioL (runMessage msg)
      >>= traverseOf entitiesL (runMessage msg)
      >>= traverseOf removedEntitiesL (runMessage msg)
      >>= traverseOf boostEntitiesL (runMessage (Boost msg))
      >>= traverseOf (activeCostL . ix 0) (\c -> asYou (activeCostIdentityId c) $ runMessage msg c)
      >>= runGameMessage msg
      >>= stalwartSweep

-- | An enemy that has gained stalwart while stunned or confused has those status cards
-- removed at once (RR Stalwart): looked for after each message, among the enemies with
-- one only.
stalwartSweep :: (HasGame m, HasQueue m) => Game -> m Game
stalwartSweep g = do
  let villains = [(VillainRef (toId v), Villain.getModifiedKeywords a, Villain.villainStunned a, Villain.villainConfused a) | v <- HashMap.elems (gameVillains g), let a = toAttrs v, Villain.villainStunned a || Villain.villainConfused a]
      minions = [(MinionRef (toId m), Minion.getModifiedKeywords a, minionStunned a, minionConfused a) | m <- HashMap.elems (gameMinions g), let a = toAttrs m, minionStunned a || minionConfused a]
  for_ (villains <> minions) \(ref, keywords, stunned, confused) -> do
    stalwart <- elem Stalwart <$> runReaderT keywords g
    when stalwart do
      when stunned $ push (RemoveStatus ref StunnedStatus)
      when confused $ push (RemoveStatus ref ConfusedStatus)
  pure g

-- | Whether the enemy, by its ref, is stalwart: it cannot be stunned or confused.
isStalwartEnemy :: (HasGame m) => Target -> m Bool
isStalwartEnemy target = do
  g <- getGame
  keywords <- case target of
    VillainRef v -> maybe (pure []) (Villain.getModifiedKeywords . toAttrs) (lookup v (gameVillains g))
    MinionRef m -> maybe (pure []) (Minion.getModifiedKeywords . toAttrs) (lookup m (gameMinions g))
    EnemyRef (EnemyVillainId v) -> maybe (pure []) (Villain.getModifiedKeywords . toAttrs) (lookup v (gameVillains g))
    EnemyRef (EnemyMinionId m) -> maybe (pure []) (Minion.getModifiedKeywords . toAttrs) (lookup m (gameMinions g))
    _ -> pure []
  pure $ Stalwart `elem` keywords

class (Monad m) => HasGame m where
  getGame :: m Game

instance (HasGame m) => Projection m PlayerIdentity where
  project fld ident = do
    pIdentity <- getIdentity ident
    pure $ field fld pIdentity

-- | What a card is played on, chosen before its cost is paid: the character an upgrade
-- attaches to, the ally Make the Call puts into play. Nothing for a card played on
-- nothing.
getPlayTargets ::
  (HasGame m, HasQueue m, MonadThrow m, Projection m PlayerIdentity) =>
  IdentityId ->
  PlayerCard ->
  m (Maybe [Target])
getPlayTargets ident card = case cdCardType (getCardDef card) of
  UpgradeType -> case createUpgrade ident card of Upgrade u -> upgradeTargets u
  EventType -> case createEvent ident card of Event e -> eventTargets e
  -- "Play under any player's control. Max N per player.": whose, among those who may
  SupportType | Just (MaxPerPlayerUnderAnyControl n) <- cdLimit (getCardDef card) -> do
    players <- getPlayers
    open <- flip filterM players \p ->
      (< n) <$> selectListCount (SupportMatches [SupportNamed (cdName $ getCardDef card), SupportControlledBy $ IdentityWithId p])
    pure $ Just $ map toTarget open
  _ -> pure Nothing

createAlly :: IdentityId -> PlayerCard -> Ally
createAlly ident card =
  lookupAlly (toCardCode card) ident (AllyId $ unCardId $ pcCardId card)

createSupport :: IdentityId -> PlayerCard -> Support
createSupport ident card =
  lookupSupport (toCardCode card) ident (SupportId $ unCardId $ pcCardId card)

createUpgrade :: IdentityId -> PlayerCard -> Upgrade
createUpgrade ident card =
  lookupUpgrade (toCardCode card) ident (UpgradeId $ unCardId $ pcCardId card)

createEvent :: IdentityId -> PlayerCard -> Event
createEvent ident card =
  lookupEvent (toCardCode card) ident (EventId $ unCardId $ pcCardId card)

createSideScheme :: EncounterCard -> SideScheme
createSideScheme card =
  lookupSideScheme (toCardCode card) (SideSchemeId $ unCardId $ ecCardId card)

createTreachery :: EncounterCard -> Treachery
createTreachery card =
  lookupTreachery (toCardCode card) (TreacheryId $ unCardId $ ecCardId card)

createObligation :: EncounterCard -> Obligation
createObligation card =
  lookupObligation (toCardCode card) (ObligationId $ unCardId $ ecCardId card)

createMinion :: IdentityId -> EncounterCard -> Minion
createMinion ident card =
  lookupMinion (toCardCode card) ident (MinionId $ unCardId $ ecCardId card)

createAttachment :: EncounterCard -> Attachment
createAttachment card =
  lookupAttachment (toCardCode card) (AttachmentId $ unCardId $ ecCardId card)

-- | The environment is in play, in the player's area.
environmentIntoPlay :: IdentityId -> Environment -> Game -> Game
environmentIntoPlay ident environment g =
  g
    & (entitiesL . environmentsL %~ insertMap (toId environment) environment)
    & intoAreaOf ident (\a -> a {areaEnvironments = areaEnvironments a <> [toId environment]})

createEnvironment :: EncounterCard -> Environment
createEnvironment card =
  lookupEnvironment (toCardCode card) (EnvironmentId $ unCardId $ ecCardId card)

createEffect :: EffectId -> CardCode -> Source -> EntityMatcher -> Effect
createEffect ident cardCode source matcher =
  lookupEffect cardCode source matcher ident

newGame :: PlayerIdentity -> Scenario -> Game
newGame player scenario =
  Game
    { gamePhase = PlayerPhase
    , gameState = Unstarted
    , gamePlayerOrder = [toId player]
    , gameBoostEntities = defaultEntities
    , gameRemovedEntities = defaultEntities
    , gameEntities =
        defaultEntities
          { entitiesPlayers = fromList [(toId player, player)]
          }
    , gameActiveCost = []
    , gameWindowDepth = 0
    , gameDepth = 0
    , gameWindows = []
    , gameQuestion = mempty
    , gameUsedAbilities = mempty
    , gameActivePlayer = toId player
    , gameYou = Nothing
    , gameScenario = scenario
    , gameFocusedCards = []
    , gamePrivateCards = mempty
    , gameBoostCards = mempty
    , gameQuestionVersion = 0
    , gameRound = 1
    , gameSeats = [toId player]
    , gamePlayedThisRound = []
    , gamePlayedThisPhase = []
    , gamePlayedThisTurn = []
    , gameTreacheriesRevealedThisPhase = []
    , gameAreas = [newArea defaultAreaId [toId player]]
    , gameSignatureSchemes = mempty
    , gameAtEndOfPhase = []
    , gameAllAreas = False
    , gameCampaign = Nothing
    , gameVictoryDisplay = []
    , gamePeril = Nothing
    , gameCollection = []
    , gameCollecting = []
    }

-- | A card with the permanent keyword cannot leave play. ponytail: none in play can be
-- removed by its own set yet, so nothing makes an exception.
isPermanent :: (HasCardDef a) => a -> Bool
isPermanent = elem Permanent . cdKeywords . getCardDef

-- | A permanent card is not a valid target for an effect that would make it leave play:
-- the effect targets another card that fits it instead (RR, Permanent). So whatever
-- asks, a choice on a permanent card in play that would discard it, remove it from play
-- or return it to hand is not offered.
withoutPermanentTargets :: Game -> Question -> Question
withoutPermanentTargets g = filterChoices (offerable g)

-- | The question with only the choices that pass.
filterChoices :: (Choice -> Bool) -> Question -> Question
filterChoices f = \case
  QuestionLabel l t q -> QuestionLabel l t (filterChoices f q)
  Searched cs q -> Searched cs (filterChoices f q)
  ChooseOne cs -> ChooseOne (filter f cs)
  ChooseN n cs -> ChooseN n (filter f cs)
  ChooseOneAtATime cs -> ChooseOneAtATime (filter f cs)
  q -> q

-- | A player engaged with a minion with patrol cannot thwart the main scheme (RR Patrol):
-- a choice that would thwart it is not theirs to make. Removing threat from it otherwise
-- than by a thwart is not kept from them.
withoutPatrolledThwarts :: Game -> Bool -> Question -> Question
withoutPatrolledThwarts g patrolled
  | patrolled = filterChoices (not . thwartsMain)
  | otherwise = id
 where
  thwartsMain = \case
    Run msgs -> any thwartMessage msgs
    Label _ cs -> any thwartsMain cs
    TargetLabel _ cs -> any thwartsMain cs
    CardLabel _ c -> thwartsMain c
    -- a thwart only by a basic thwart or an ability labeled one, as 'resolveChoice' says
    ThwartScheme t source _ -> isMain t && labeledThwart source
    BasicThwartScheme t _ _ -> isMain t
    _ -> False
  thwartMessage = \case
    MainSchemeMessage _ (MainSchemeThwarted _ _) -> True
    _ -> False
  isMain = \case
    MainSchemeRef _ -> True
    SchemeRef (SchemeMainSchemeId _) -> True
    _ -> False
  labeledThwart source = maybe False (elem Ability.Thwart . cdAbilitySubTypes) (abilityCardDef g (toTarget source))

-- | The choice is not one that would make a permanent card leave play.
offerable :: Game -> Choice -> Bool
offerable g = keep
 where
  keep = \case
    TargetLabel t cs | permanent t -> not (any (removes t) (concatMap runs cs))
    CardLabel _ c -> keep c
    _ -> True
  runs = \case
    Run msgs -> msgs
    Label _ cs -> concatMap runs cs
    TargetLabel _ cs -> concatMap runs cs
    CardLabel _ c -> runs c
    _ -> []
  removes t = \case
    RemoveFromPlay t' -> t' == t
    RemoveFromGame t' -> t' == t
    ReturnToHand t' -> t' == t
    UpgradeMessage uid DiscardUpgrade -> t == UpgradeRef uid
    _ -> False
  permanent = \case
    UpgradeRef uid -> maybe False isPermanent (lookup uid (gameUpgrades g))
    SupportRef sid -> maybe False (elem Permanent . cdKeywords . supportCardDef . toAttrs) (lookup sid (gameSupports g))
    AllyRef aid -> maybe False isPermanent (lookup aid (gameAllies g))
    _ -> False

-- | The campaign the game is a scenario of, if it is one.
getCampaign :: (HasGame m) => m (Maybe Campaign)
getCampaign = getsGame gameCampaign

-- | The player's identity as a campaign log knows it: its hero face's code.
getIdentityCode :: (HasGame m) => IdentityId -> m CardCode
getIdentityCode ident = getsGame \g -> maybe (error "no such player") (toHeroCardCode . toCardCode) $ lookup ident (gamePlayers g)

-- | The identity's hit points: left, the most it has, and printed.
getRemainingHitPoints :: (HasGame m) => IdentityId -> m (Natural, Natural, Natural)
getRemainingHitPoints ident = do
  p <- getIdentity ident
  hp <- getModifiedHp p
  pure (subtractNatural (identityDamage p) hp, hp, unHp (playerIdentityHP $ toAttrs p))

-- | The counters on the main schemes in play.
getCountersOnMainSchemes :: (HasGame m) => m Natural
getCountersOnMainSchemes = getsGame \g -> sum [mainSchemeCounters (toAttrs m) | m <- HashMap.elems (gameMainSchemes g)]

-- | The side schemes in play, by code, each with the player cards beneath it.
getSideSchemesInPlay :: (HasGame m) => m [(CardCode, [PlayerCard])]
getSideSchemesInPlay = getsGame $ map (\s -> (cdCardCode (getCardDef s), sideSchemeHeldCards (toAttrs s))) . HashMap.elems . gameSideSchemes

-- | The cards the player owns in their hand, deck and discard pile, and the allies they
-- own in play.
getCardsOwnedBy :: (HasGame m) => IdentityId -> m [CardDef]
getCardsOwnedBy ident = getsGame \g ->
  [ getCardDef c
  | Just p <- [lookup ident (gamePlayers g)]
  , let a = toAttrs p
  , c <- unHand a.playerIdentityHand <> unDeck a.playerIdentityDeck <> unDiscard a.playerIdentityDiscard
  ]
    <> [getCardDef ally | ally <- HashMap.elems (gameAllies g), allyOwner (toAttrs ally) == ident]

-- | A game with a seat for each player, in the order given.
newTable :: Scenario -> NonEmpty PlayerIdentity -> Game
newTable scenario (player :| others) = foldl' seat (newGame player scenario) others
 where
  seat g p =
    g
      & (entitiesL . playersL %~ insertMap (toId p) p)
      & (playerOrderL <>~ [toId p])
      & (seatsL <>~ [toId p])
      & joinFirstArea (toId p)

-- | A player sitting down plays in the first area.
joinFirstArea :: IdentityId -> Game -> Game
joinFirstArea ident = areasL %~ \case
  a : as -> a {areaPlayers = areaPlayers a <> [ident]} : as
  [] -> [newArea defaultAreaId [ident]]

addPlayer :: (MonadGame env m) => PlayerIdentity -> m ()
addPlayer player =
  withGame_ $
    (entitiesL . playersL %~ insertMap (toId player) player)
      . (playerOrderL <>~ [toId player])
      . joinFirstArea (toId player)

withGame :: (MonadGame env m) => (Game -> (Game, a)) -> m a
withGame f = do
  gameRef <- getGameRef
  liftIO $ atomicModifyIORef' gameRef f

withGame_ :: (MonadGame env m) => (Game -> Game) -> m ()
withGame_ f = withGame ((,()) . f)

withGameM :: (MonadGame env m) => (Game -> m Game) -> m ()
withGameM f = getGame >>= f >>= withGame_ . const

getsGame :: (HasGame m) => (Game -> a) -> m a
getsGame f = f <$> getGame

-- | The encounter deck, its top card first.
-- | The boost icons on the encounter card: its own, and those The Next Evolution adds.
getBoostIcons :: (HasGame m) => EncounterCard -> m Natural
getBoostIcons card = do
  extra <- sum . map (\case AdditionalBoostIcons n -> n; _ -> 0) <$> getModifiers GameRef
  pure $ boostCount (cdBoostIcons $ getCardDef card) + extra

getEncounterDeck :: (HasGame m) => m [EncounterCard]
getEncounterDeck = fmap pileDeck . getPile =<< getDeckKeyFor =<< getYou

getEncounterDiscard :: (HasGame m) => m [EncounterCard]
getEncounterDiscard = fmap pileDiscard . getPile =<< getDeckKeyFor =<< getYou

-- | The cards played this round, and by whom.
getPlayedThisRound :: (HasGame m) => m [(IdentityId, CardDef)]
getPlayedThisRound = getsGame gamePlayedThisRound

getPlayedThisPhase :: (HasGame m) => m [(IdentityId, CardDef)]
getPlayedThisPhase = getsGame gamePlayedThisPhase

getPlayedThisTurn :: (HasGame m) => m [(IdentityId, CardDef)]
getPlayedThisTurn = getsGame gamePlayedThisTurn

getTreacheriesRevealedThisPhase :: (HasGame m) => m [(IdentityId, TreacheryId)]
getTreacheriesRevealedThisPhase = getsGame gameTreacheriesRevealedThisPhase

getPhase :: (HasGame m) => m Phase
getPhase = getsGame gamePhase

getUsedAbilities :: (HasGame m) => m (HashMap IdentityId [Ability])
getUsedAbilities = getsGame (HashMap.map (map fst) . view usedAbilitiesL)

-- | Whether a player may use an ability again under its limit. An ability on an
-- encounter card is anyone's to trigger, so its limit counts every player's uses (a
-- forced ability used once is not offered to the next), unless it is a limit per player.
withinUseLimit :: (HasGame m) => IdentityId -> Ability -> m Bool
withinUseLimit ident a = getsGame \g -> do
  let used = HashMap.map (map fst) (gameUsedAbilities g)
      pooled = isNothing (abilityController g a) && not (perPlayer (abilityLimit a))
      perPlayer = \case
        EachPlayer _ -> True
        _ -> False
  passesUseLimit ident (if pooled then HashMap.singleton ident (concat $ HashMap.elems used) else used) a

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

class HasGameRef m where
  getGameRef :: m (IORef Game)

instance (Monad m) => HasGame (ReaderT Game m) where
  getGame = ask

instance (HasQueue m) => HasQueue (ReaderT Game m) where
  getQueue = lift getQueue
  pushAll = lift . pushAll

getIdentity :: (HasCallStack, HasGame m) => IdentityId -> m PlayerIdentity
getIdentity ident = do
  g <- getGame
  case lookup ident (gamePlayers g) of
    Just a -> pure a
    Nothing -> error "missing identity"

initPlayer :: (MonadRandom m) => CardCode -> Deck -> m PlayerIdentity
initPlayer cardCode deck = do
  ident <- getRandom
  let
    mAlterEgo = do
      def <- lookup (toAlterEgoCardCode cardCode) allAlterEgosMap
      lookupAlterEgo def ident
    mHero = do
      def <- lookup (toHeroCardCode cardCode) allHeroesMap
      lookupHero def ident
    -- Ant-Man's fold-out card: a second hero form on side C
    addThirdSide p@(PlayerIdentity a) = case lookup (flipToSide C cardCode) allHeroesMap >>= (`lookupHero` ident) of
      Just giant -> PlayerIdentity a {playerIdentitySides = HashMap.insert C giant (playerIdentitySides a)}
      Nothing -> p
  case (mAlterEgo, mHero) of
    (Just alterEgoSide, Just heroSide) ->
      pure . setDeck deck . addThirdSide $ createIdentity ident alterEgoSide heroSide
    _ -> error "stuff"

createPlayer :: (MonadGame env m) => CardCode -> Deck -> m ()
createPlayer cardCode deck = do
  playerIdentity <- initPlayer cardCode deck
  addPlayer playerIdentity

instance HasAbilities Game where
  getAbilities g =
    concatMap getAbilities (HashMap.elems $ gamePlayers g)
      <> concatMap getAbilities (HashMap.elems $ gameAllies g)
      <> concatMap getAbilities (HashMap.elems $ gameSupports g)
      <> concatMap getAbilities (HashMap.elems $ gameUpgrades g)
      <> concatMap getAbilities (HashMap.elems $ gameVillains g)
      <> concatMap getAbilities (HashMap.elems $ gameAttachments g)
      <> concatMap getAbilities (HashMap.elems $ gameMinions g)
      <> concatMap getAbilities (HashMap.elems $ gameEnvironments g)
      <> concatMap getAbilities (HashMap.elems $ gameObligations g)
      <> concatMap getAbilities (HashMap.elems $ gameSideSchemes g)
      <> concatMap getAbilities (HashMap.elems $ gameMainSchemes g)

-- | The def of the player card an ability is on: in play, or an upgrade just discarded
-- to pay for it.
abilityCardDef :: Game -> Target -> Maybe CardDef
abilityCardDef g = \case
  UpgradeRef uid ->
    getCardDef <$> (lookup uid (gameUpgrades g) <|> lookup uid (g ^. removedEntitiesL . upgradesL))
  SupportRef sid -> supportCardDef . toAttrs <$> (lookup sid (gameSupports g) <|> lookup sid (g ^. removedEntitiesL . supportsL))
  AllyRef aid -> getCardDef <$> (lookup aid (gameAllies g) <|> lookup aid (g ^. removedEntitiesL . alliesL))
  VillainRef vid -> getCardDef <$> lookup vid (gameVillains g)
  EventRef eid -> eventCardDef . toAttrs <$> lookup eid (gameEvents g)
  _ -> Nothing

-- | Who may trigger an ability: the player controlling the card it is on. Abilities on
-- encounter cards are anyone's.
abilityController :: Game -> Ability -> Maybe IdentityId
abilityController g a
  -- "for any player": the card's controller resolves it for whoever pays
  | forAnyPlayer (abilityCriteria a) = Nothing
  | otherwise = cardController g a

-- | The player controlling the card an ability is on, if a player does.
cardController :: Game -> Ability -> Maybe IdentityId
cardController g a = case abilitySource a of
  IdentityRef ident -> Just ident
  AllyRef aid -> getAllyController <$> lookup aid (gameAllies g)
  SupportRef sid -> getSupportController <$> lookup sid (gameSupports g)
  UpgradeRef uid -> getUpgradeController <$> lookup uid (gameUpgrades g)
  -- only the player whose identity an attachment is on, or whose play area an
  -- obligation is in, may trigger it (RR, Attachment and Obligation)
  AttachmentRef aid -> Attachment.attachmentIdentity . toAttrs =<< lookup aid (gameAttachments g)
  ObligationRef oid -> Obligation.obligationOwner . toAttrs =<< lookup oid (gameObligations g)
  _ -> Nothing

-- | The player controlling the card a source names, if a player does.
getSourceController :: (HasGame m) => Source -> m (Maybe IdentityId)
getSourceController source = getsGame \g -> case source of
  IdentityRef ident -> Just ident
  AllyRef aid -> getAllyController <$> lookup aid (gameAllies g)
  SupportRef sid -> getSupportController <$> lookup sid (gameSupports g)
  UpgradeRef uid -> getUpgradeController <$> lookup uid (gameUpgrades g)
  EventRef eid -> eventController . toAttrs <$> lookup eid (gameEvents g)
  ObligationRef oid -> obligationOwner . toAttrs =<< lookup oid (gameObligations g)
  _ -> Nothing

-- | The abilities in play a player may trigger.
getAbilitiesFor :: (HasGame m) => IdentityId -> m [Ability]
getAbilitiesFor ident = do
  abilities <- getsGame \g -> filter (\a -> maybe True (== ident) (abilityController g a) && unhelped g a) (getAbilities g)
  filterM (\a -> if printed a then not <$> isBlanked (abilitySource a) else pure True) abilities
 where
  -- a blank text box leaves the game's own abilities: basic powers and changing form
  printed a = abilityType a /= Basic && not (isIdentity (abilitySource a) && abilityIndex a == 100)
  isIdentity = \case
    IdentityRef _ -> True
    _ -> False
  -- while a player resolves a card with peril, the others trigger nothing but what is
  -- forced, and no card another player controls helps them (RR Peril)
  unhelped g a = case gamePeril g of
    Just p | p /= ident -> isForcedAbility a
    Just _ -> maybe True (== ident) (cardController g a)
    Nothing -> True

-- | Whether the card's printed text box is treated as blank (Tech Theft). Only the cards
-- that can blank another are asked, so a blanked card's own modifiers are not needed.
isBlanked :: (HasGame m, IsRef a) => a -> m Bool
isBlanked a = do
  g <- getGame
  elem Blank . mconcat
    <$> sequence
      [ concatMapM (getModifiersFor (toSource a) (toTarget a)) (HashMap.elems $ gameSideSchemes g)
      , concatMapM (getModifiersFor (toSource a) (toTarget a)) (HashMap.elems $ gameEffects g)
      , concatMapM (getModifiersFor (toSource a) (toTarget a)) (HashMap.elems $ gameAttachments g)
      , concatMapM (getModifiersFor (toSource a) (toTarget a)) (HashMap.elems $ gameEnvironments g)
      , concatMapM (getModifiersFor (toSource a) (toTarget a)) (HashMap.elems $ gameVillains g)
      ]

-- | Run messages until the queue empties or someone has to answer a question, saying
-- nothing: for games played out of sight, such as a look ahead.
runGameMessages :: (MonadGame env m) => m ()
runGameMessages = runGameMessagesWith (const $ pure ())

-- | Run messages until the queue empties or someone has to answer a question, telling
-- the function given about each message before it runs: that is where the log is told.
runGameMessagesWith :: (MonadGame env m) => (Message -> m ()) -> m ()
runGameMessagesWith before = go (0 :: Int)
 where
  go n = do
    g <- getGame
    if
      | notNull (gameQuestion g) || isFinished g -> pure ()
      | n >= messageLimit -> error "message limit reached: the game is probably looping"
      | otherwise ->
          pop >>= \case
            Nothing -> pure ()
            Just (Ask ident asked) -> isPatrolled ident >>= \patrolled -> case withoutPatrolledThwarts g patrolled (withoutPermanentTargets g asked) of
              -- nothing is asked when there is nothing to choose
              question | not (hasChoices question) -> go (n + 1)
              question -> do
                withGame_ $ (questionL .~ fromList [(ident, question)]) . (questionVersionL +~ 1)
                -- a player chosen when there is only the one is chosen for them
                when (onlyPlayer question) $ answerQuestionWith before ident 0
            Just other -> do
              debug other
              before other
              withGameM $ runMessage other
              go (n + 1)
  isFinished g = case gameState g of
    Finished _ -> True
    _ -> False

messageLimit :: Int
messageLimit = 10000

-- | Whether all the question offers is one player.
onlyPlayer :: Question -> Bool
onlyPlayer = \case
  QuestionLabel _ _ q -> onlyPlayer q
  Read _ _ -> False
  Searched _ q -> onlyPlayer q
  ChooseOne [TargetLabel (IdentityRef _) _] -> True
  ChoosePlayerOrder (Unsorted [_]) _ -> True
  _ -> False

-- | Answer the question a player was asked with the choice at the index given, and play
-- on to the next question.
answerQuestionWith :: (MonadGame env m) => (Message -> m ()) -> IdentityId -> Int -> m ()
answerQuestionWith before ident n = do
  g <- getGame
  question <- maybe (throwM NoQuestion) pure $ lookup ident (gameQuestion g)
  withGame_ $ questionL .~ mempty
  (msgs, remaining) <- answer question
  pushAll $ msgs <> [Ask ident q | Just q <- [remaining]]
  runGameMessagesWith before
 where
  -- what the choice does, and what is left to ask
  answer = \case
    QuestionLabel l t q -> second (fmap (QuestionLabel l t)) <$> answer q
    Read r q -> second (fmap (Read r)) <$> answer q
    Searched cs q -> second (fmap (Searched cs)) <$> answer q
    ChooseOne qs -> (,Nothing) <$> pick qs
    ChooseN k qs -> do
      msgs <- pick qs
      let rest = without qs
      pure (msgs, guard (k > 1 && notNull rest) $> ChooseN (k - 1) rest)
    ChooseOneAtATime qs -> do
      msgs <- pick qs
      let rest = without qs
      pure (msgs, guard (notNull rest) $> ChooseOneAtATime rest)
    ChoosePlayerOrder (Unsorted unsorted) (Sorted sorted) -> case atMay unsorted n of
      Nothing -> throwM InvalidChoice
      Just chosen -> do
        let sorted' = sorted <> [chosen]
        pure case filter (/= chosen) unsorted of
          [] -> ([SetPlayerOrder sorted'], Nothing)
          [lastOne] -> ([SetPlayerOrder (sorted' <> [lastOne])], Nothing)
          rest -> ([], Just $ ChoosePlayerOrder (Unsorted rest) (Sorted sorted'))
  pick qs = maybe (throwM InvalidChoice) (choiceMessages ident) (atMay qs n)
  without qs = take n qs <> drop (n + 1) qs

-- | Run a debug message now, even while a question waits: it and whatever it leads to go
-- first, then the waiting question is asked again and play carries on from there.
runDebugWith :: (Message -> GameAppT ()) -> Message -> GameAppT ()
runDebugWith before msg = do
  asked <- getsGame gameQuestion
  withGame_ $ questionL .~ mempty
  pushAll $ msg : [Ask ident q | (ident, q) <- HashMap.toList asked]
  runGameMessagesWith before

-- TODO: implement this for api
replayChoices :: (MonadGame env m) => [Diff.Patch] -> m ()
replayChoices _ = pure ()

-- | The cards a player may discard to pay: those in their own hand. A resource card's
-- criteria say when it may be spent: "spend this card only in hero form".
getAvailablePaymentSources ::
  (HasQueue m, MonadThrow m, HasGame m, Projection m PlayerIdentity) => IdentityId -> m [PlayerCard]
getAvailablePaymentSources ident = asYou ident do
  hand <- getsGame $ maybe [] (unHand . playerIdentityHand . toAttrs) . lookup ident . gamePlayers
  flip filterM hand \c ->
    if cdCardType (getCardDef c) == ResourceType
      then passesCardCriteria ident c (cdCriteria $ getCardDef c)
      else pure True

getAvailableResourcesFor ::
  ( HasCallStack
  , HasQueue m
  , MonadRandom m
  , MonadThrow m
  , HasGame m
  , Projection m PlayerIdentity
  ) =>
  IdentityId ->
  Maybe PlayerCard ->
  m [Resource]
getAvailableResourcesFor ident mc = do
  abilitiesResources <- concatMapM abilityResources =<< getResourceAbilities ident mc
  playerResources <- concatMapM (`resourcesFor` mc) =<< getAvailablePaymentSources ident
  pure $ playerResources <> abilitiesResources

getResourceAbilities ::
  ( MonadThrow m
  , MonadRandom m
  , HasQueue m
  , HasGame m
  ) =>
  IdentityId ->
  -- | the card being paid for, if a card is
  Maybe PlayerCard ->
  m [Ability]
getResourceAbilities ident mc = asYou ident do
  abilities <- getAbilitiesFor ident
  filterM
    ( andM
        . sequence
          [ withinUseLimit ident
          , pure . (`elem` [Resource, HeroResource, AlterEgoResource]) . abilityType
          , pure . paysFor
          , passesCanAffordCost ident
          , passesCriteria ident
          ]
    )
    abilities
 where
  paysFor a = all (\m -> maybe False (cardMatch m) mc) (payingForMatchers $ abilityCriteria a)

gameSelectIdentity :: (HasGame m) => IdentityMatcher -> m (HashSet IdentityId)
gameSelectIdentity m = do
  everyone <- HashMap.elems <$> getsGame gamePlayers
  identities <- if named m then pure everyone else inYourAreaOnly (IdentityRef . toId) everyone
  result <- filterM (matchFilter m) identities
  pure $ HashSet.fromList $ map toId result
 where
  -- a player named, or the first player, is who they are in any area
  named = \case
    FirstPlayer -> True
    You -> True
    IdentityWithId _ -> True
    IdentityMatchAll xs -> any named xs
    _ -> False
  matchFilter x = case x of
    AnyIdentity -> pure . const True
    HeroIdentity -> pure . isHero
    AlterEgoIdentity -> pure . isAlterEgo
    UnexhaustedIdentity -> pure . not . isExhausted
    Matchers.ExhaustedIdentity -> pure . isExhausted
    ConfusedIdentity -> pure . identityIsConfused
    StunnedIdentity -> pure . identityIsStunned
    IdentityEngagedWith minionMatcher -> \ident -> do
      minions <- select minionMatcher
      pure
        . not
        . null
        $ playerIdentityMinions (toAttrs ident)
          `HashSet.intersection` minions
    IdentityWithId ident' -> pure . (== ident') . toId
    IdentityWithTrait trait -> fmap (member trait) . getTraits
    IdentityWithTitle t -> pure . (== t) . title . cdName . getCardDef
    IdentityIs def -> pure . (== toBaseCardCode (cdCardCode def)) . toBaseCardCode . toCardCode
    IdentityWithDamage gameValueMatcher ->
      gameValueMatches gameValueMatcher . identityDamage
    IdentityWithCounters gameValueMatcher ->
      gameValueMatches gameValueMatcher . playerIdentityCounters . toAttrs
    IdentityMatchAll xs -> andM . traverse matchFilter xs
    IdentityCanThwart -> fmap (notElem CannotThwart) . getModifiers
    IdentityCanChangeForm -> fmap (notElem CannotChangeForm) . getModifiers
    FirstPlayer -> \a -> do
      ps <- getAllPlayers
      pure $ case ps of
        fp : _ -> toId a == fp
        _ -> False
    You -> \a -> (== toId a) <$> getYou
    IdentityWithFewestRemainingHitPoints m' -> \a -> do
      candidates <- filterM (matchFilter m') . HashMap.elems =<< getsGame gamePlayers
      remaining <- for candidates \p -> (toId p,) . subtractNatural (identityDamage p) <$> getModifiedHp p
      pure $ case find ((== toId a) . fst) remaining of
        Just (_, mine) -> all ((>= mine) . snd) remaining
        Nothing -> False

gameSelectCharacter :: (HasGame m) => CharacterMatcher -> m (HashSet CharacterId)
gameSelectCharacter = \case
  CharacterWithDamage gameValueMatcher -> do
    villains <-
      map VillainCharacter
        <$> selectList (VillainWithDamage gameValueMatcher)
    minions <-
      map MinionCharacter
        <$> selectList (MinionWithDamage gameValueMatcher)
    identities <-
      map IdentityCharacter
        <$> selectList (IdentityWithDamage gameValueMatcher)
    allies <- map AllyCharacter <$> selectList (AllyWithDamage gameValueMatcher)
    pure . HashSet.fromList $ villains <> minions <> identities <> allies
  DamageableCharacter -> do
    let
      enemyToCharacter = \case
        EnemyMinionId mid -> MinionCharacter mid
        EnemyVillainId vid -> VillainCharacter vid
    enemies <- selectMap enemyToCharacter DamageableEnemy
    identities <- selectMap IdentityCharacter AnyIdentity
    allies <- selectMap AllyCharacter AnyAlly
    pure . HashSet.fromList $ enemies <> identities <> allies
  AnyCharacter -> do
    villains <- selectMap VillainCharacter AnyVillain
    minions <- selectMap MinionCharacter AnyMinion
    identities <- selectMap IdentityCharacter AnyIdentity
    allies <- selectMap AllyCharacter AnyAlly
    pure . HashSet.fromList $ villains <> minions <> identities <> allies
  IdentityCharacters identityMatcher -> HashSet.fromList <$> selectMap IdentityCharacter identityMatcher
  CharacterControlledBy identityMatcher -> do
    identities <- selectMap IdentityCharacter identityMatcher
    allies <- selectMap AllyCharacter (AllyControlledBy identityMatcher)
    pure . HashSet.fromList $ identities <> allies
  CharacterWithTrait t -> do
    villains <- getsGame $ map (VillainCharacter . toId) . filter (member t . cdTraits . getCardDef) . HashMap.elems . gameVillains
    minions <- selectMap MinionCharacter (MinionWithTrait t)
    identities <- selectMap IdentityCharacter (IdentityWithTrait t)
    allies <- selectMap AllyCharacter (AllyWithTrait t)
    pure . HashSet.fromList $ villains <> minions <> identities <> allies
  CharacterWithStatus s -> getsGame \g ->
    let
      -- each character's stunned, confused and tough
      has :: (a -> (Bool, Bool, Bool)) -> (a -> CharacterId) -> HashMap k a -> [CharacterId]
      has statuses f = map f . filter (pick . statuses) . HashMap.elems
      pick (stunned, confused, tough) = case s of
        StunnedStatus -> stunned
        ConfusedStatus -> confused
        ToughStatus -> tough
     in
      HashSet.fromList $
        has ((\a -> (a.playerIdentityStunned, a.playerIdentityConfused, a.playerIdentityTough)) . toAttrs) (IdentityCharacter . toId) (gamePlayers g)
          <> has ((\a -> (a.allyStunned, a.allyConfused, a.allyTough)) . toAttrs) (AllyCharacter . toId) (gameAllies g)
          <> has ((\a -> (a.villainStunned, a.villainConfused, a.villainTough)) . toAttrs) (VillainCharacter . toId) (gameVillains g)
          <> has ((\a -> (a.minionStunned, a.minionConfused, a.minionTough)) . toAttrs) (MinionCharacter . toId) (gameMinions g)
  HeroOrAllyWithFewestRemainingHitPoints -> do
    heroes <- traverse (\p -> (IdentityCharacter (toId p),) . (`subtractNatural'` (toAttrs p).playerIdentityDamage) <$> getModifiedHp p) . filter isHero . HashMap.elems =<< getsGame gamePlayers
    allies <- traverse (\a -> (AllyCharacter (toId a),) . (`subtractNatural'` getAllyDamage a) <$> Ally.getModifiedHitPoints (toAttrs a)) . HashMap.elems =<< getsGame gameAllies
    let characters = heroes <> allies
        fewest = minimum (map snd characters)
    pure . HashSet.fromList $ [c | not (null characters), (c, hp) <- characters, hp == fewest]
   where
    subtractNatural' hp dmg = subtractNatural dmg hp
  HeroOrAllyWithHighestAttack -> do
    heroes <- traverse (\p -> (IdentityCharacter (toId p),) <$> getIdentityHeroAttackDamage p) . filter isHero . HashMap.elems =<< getsGame gamePlayers
    allies <- traverse (\a -> (AllyCharacter (toId a),) <$> Ally.getModifiedAttack (toAttrs a)) . HashMap.elems =<< getsGame gameAllies
    let characters = heroes <> allies
        highest = maximum (map snd characters)
    pure . HashSet.fromList $ [c | not (null characters), (c, atk) <- characters, atk == highest]
  CharacterMatches [] -> pure mempty
  CharacterMatches (x : xs) ->
    foldl' HashSet.intersection
      <$> gameSelectCharacter x
      <*> traverse gameSelectCharacter xs

gameSelectExtendedCard ::
  (MonadThrow m, HasGame m, HasQueue m, Projection m PlayerIdentity) =>
  ExtendedCardMatcher ->
  m (HashSet PlayerCard)
gameSelectExtendedCard m = do
  let
    excludedCards = getExcludedCards m
    msgs = map (RemoveFromGame . toRef) excludedCards
  players <- HashMap.elems <$> getsGame gamePlayers
  g <- getGame
  tempApp <- newApp g msgs
  let
    allCards =
      concatMap (unHand . playerIdentityHand . toAttrs) players
        <> concatMap (unDiscard . playerIdentityDiscard . toAttrs) players
        <> concatMap (unDeck . playerIdentityDeck . toAttrs) players
  result <- runGameApp tempApp $ do
    runGameMessages
    go players allCards m
  pure $ HashSet.fromList result
 where
  getExcludedCards = \case
    NotCard c -> [c]
    ExtendedCardMatches ms -> concatMap getExcludedCards ms
    _ -> []
  go players cards = \case
    NotCard c -> pure $ filter (/= c) cards
    AffordableCardBy identityMatcher extendedCardMatcher -> do
      identities <- selectList identityMatcher
      let players' = filter ((`elem` identities) . toId) players
      -- the cards may be anyone's: only who affords them is narrowed
      inner <- go players cards extendedCardMatcher
      concatMapM (\ident -> filterM (isPlayable ident) inner) players'
    PlayableIgnoringCostBy identityMatcher extendedCardMatcher -> do
      identities <- selectList identityMatcher
      let players' = filter ((`elem` identities) . toId) players
      inner <- go players cards extendedCardMatcher
      concatMapM (\ident -> filterM (isPlayableWith True ident) inner) players'
    BasicCardMatches cardMatcher -> do
      pure $ filter (cardMatch cardMatcher) cards
    InDiscardOf identityMatcher extendedCardMatcher -> do
      identities <- selectList identityMatcher
      let
        players' = filter ((`elem` identities) . toId) players
        cards' =
          filter (`elem` cards) $
            concatMap (unDiscard . playerIdentityDiscard . toAttrs) players'
      go players cards' extendedCardMatcher
    InHandOf identityMatcher extendedCardMatcher -> do
      identities <- selectList identityMatcher
      let
        cards' =
          filter (`elem` cards) $
            concatMap (unHand . playerIdentityHand . toAttrs) $
              filter ((`elem` identities) . toId) players
      go players cards' extendedCardMatcher
    TopmostCardInDiscardOf identityMatcher cardMatcher -> do
      identities <- selectList identityMatcher
      let players' = filter ((`elem` identities) . toId) players
      pure $
        mapMaybe
          ( find (and . sequence [(`elem` cards), cardMatch cardMatcher])
              . unDiscard
              . playerIdentityDiscard
              . toAttrs
          )
          players'
    ExtendedCardMatches matchers -> foldlM (go players) cards matchers
    CanEnterPlay -> do
      inPlay <- getUniqueTitlesInPlay
      let named c = (title $ cdName $ getCardDef c, maybe [] pure $ subtitle $ cdName $ getCardDef c)
      pure $ filter (\c -> not (cdUnique $ getCardDef c) || not (any (uniqueMatch (named c)) inPlay)) cards

gameSelectEncounterCard ::
  (MonadRandom m, HasGame m) =>
  EncounterCardMatcher ->
  m (HashSet EncounterCard)
gameSelectEncounterCard m = case m of
  -- the nemesis set the player set aside during setup, as much of it as is still there
  NemesisSetFor identityId -> do
    mIdentity <- getsGame $ lookup identityId . entitiesPlayers . gameEntities
    setAside <- getsGame $ onlyEncounterCards . scenarioSetAsideCards . toAttrs . gameScenario
    pure $ case mIdentity of
      Nothing -> mempty
      Just iid ->
        HashSet.fromList $
          filter ((== Just (getNemesisSet $ toCardCode iid)) . cdEncounterSet . getCardDef) setAside

gameSelectAlly :: (HasGame m) => AllyMatcher -> m (HashSet AllyId)
gameSelectAlly m = do
  allies <- inYourAreaOnly (AllyRef . toId) . HashMap.elems =<< getsGame gameAllies
  result <- filterM (matchFilter m) allies
  pure $ HashSet.fromList $ map toId result
 where
  matchFilter x = case x of
    NotAlly m' -> fmap not . matchFilter m'
    AnyAlly -> pure . const True
    UnexhaustedAlly -> pure . not . isExhausted
    ExhaustedAlly -> pure . isExhausted
    AllyControlledBy identityMatcher -> \ally -> do
      identities <- select identityMatcher
      pure $ member (getAllyController ally) identities
    AllyWithUses gameValueMatcher ->
      gameValueMatches gameValueMatcher . getAllyUses
    AllyWithDamage gameValueMatcher ->
      gameValueMatches gameValueMatcher . getAllyDamage
    AllyWithId ident' -> pure . (== ident') . toId
    AllyMatches xs -> andM . traverse matchFilter xs
    AllyWithTrait trait -> fmap (member trait) . getTraits
    AllyIs def -> pure . sameCard def . getCardDef
    AllyWithTitle t -> pure . (== t) . title . cdName . getCardDef
    AllyNamedAs t -> pure . (\n -> title n == t || subtitle n == Just t) . cdName . getCardDef
    AllyWithRemainingHitPoints n -> \ally -> (== n) . subtractNatural (getAllyDamage ally) <$> Ally.getModifiedHitPoints (toAttrs ally)
    AllyWithUpgrade u -> \ally ->
      selectAny $ u <> UpgradeOneOf (map UpgradeWithId . toList . allyUpgrades $ toAttrs ally)

gameSelectSupport :: (HasGame m) => SupportMatcher -> m (HashSet SupportId)
gameSelectSupport = \case
  UnexhaustedSupport -> do
    supports <- HashMap.elems <$> getsGame gameSupports
    pure $ HashSet.fromList $ map toId $ filter (not . isExhausted) supports
  SupportControlledBy identityMatcher -> do
    supports <- HashMap.elems <$> getsGame gameSupports
    identities <- select identityMatcher
    pure $
      HashSet.fromList $
        map toId $
          filter
            ((`member` identities) . getSupportController)
            supports
  SupportWithUses gameValueMatcher -> do
    upgrades <- HashMap.elems <$> getsGame gameSupports
    HashSet.fromList
      . map toId
      <$> filterM (gameValueMatches gameValueMatcher . getSupportUses) upgrades
  SupportNamed name -> do
    supports <- HashMap.elems <$> getsGame gameSupports
    pure $ HashSet.fromList $ map toId $ filter ((== name) . cdName . supportCardDef . toAttrs) supports
  SupportMatches xs -> do
    all' <- HashSet.fromList . HashMap.keys <$> getsGame gameSupports
    foldr HashSet.intersection all' <$> traverse gameSelectSupport xs
  SupportWithTrait t -> do
    supports <- HashMap.elems <$> getsGame gameSupports
    pure $ HashSet.fromList $ map toId $ filter (member t . cdTraits . supportCardDef . toAttrs) supports

gameSelectUpgrade :: (HasGame m) => UpgradeMatcher -> m (HashSet UpgradeId)
gameSelectUpgrade m = do
  upgrades <- HashMap.elems <$> getsGame gameUpgrades
  result <- filterM (matchFilter m) upgrades
  pure $ HashSet.fromList $ map toId result
 where
  matchFilter x = case x of
    UnexhaustedUpgrade -> pure . not . isExhausted
    UpgradeWithUses gameValueMatcher ->
      gameValueMatches gameValueMatcher . getUpgradeUses
    UpgradeWithTrait t -> pure . member t . cdTraits . getCardDef
    UpgradeWithKeyword k -> pure . member k . cdKeywords . getCardDef
    UpgradeControlledBy identityMatcher -> \upgrade -> do
      identities <- select identityMatcher
      pure $ getUpgradeController upgrade `member` identities
    UpgradeMatches xs -> andM . traverse matchFilter xs
    UpgradeIs def -> pure . sameCard def . getCardDef
    UpgradeWithCard m -> pure . cardMatch m . getCardDef
    UpgradeNamed name -> pure . (== name) . cdName . getCardDef
    UpgradeWithId upgradeId -> pure . (== upgradeId) . toId
    UpgradeOneOf xs -> orM . traverse matchFilter xs
    NotUpgrade m' -> fmap not . matchFilter m'
    UpgradeOnIdentity identityMatcher -> \upgrade -> do
      identities <- select identityMatcher
      let a = toAttrs upgrade
      pure $
        getUpgradeController upgrade `member` identities
          && isNothing (upgradeAttachedAlly a)
          && isNothing (upgradeAttachedEnemy a)
          && isNothing (upgradeAttachedSideScheme a)
          && isNothing (upgradeAttachedMainScheme a)
    UpgradeThatCanLeavePlay -> pure . not . isPermanent
    UpgradeWithHighestCostAmong m' -> \upgrade -> do
      candidates <- filterM (matchFilter m') . HashMap.elems =<< getsGame gameUpgrades
      let cost = fromMaybe 0 . cdCost . getCardDef
      pure $ toId upgrade `elem` map toId candidates && cost upgrade == maximum (map cost candidates)

gameSelectAttachment ::
  (HasGame m) => AttachmentMatcher -> m (HashSet AttachmentId)
gameSelectAttachment m = do
  attachments <- HashMap.elems <$> getsGame gameAttachments
  result <- filterM (matchFilter m) attachments
  pure $ HashSet.fromList $ map toId result
 where
  matchFilter x = case x of
    AttachmentWithId ident' -> pure . (== ident') . toId
    AttachmentIs def -> pure . sameCard def . getCardDef
    AttachmentWithTrait t -> pure . member t . cdTraits . getCardDef
    AttachmentOnEnemy eid -> pure . (== Just eid) . Attachment.attachmentEnemy . toAttrs
    AttachmentMatches xs -> \a -> allM (`matchFilter` a) xs

gameSelectEnvironment ::
  (HasGame m) => EnvironmentMatcher -> m (HashSet EnvironmentId)
gameSelectEnvironment m = do
  environments <- HashMap.elems <$> getsGame gameEnvironments
  pure . HashSet.fromList . map toId $ filter matches environments
 where
  matches e = case m of
    AnyEnvironment -> True
    EnvironmentIs def -> sameCard def (getCardDef e)
    EnvironmentWithTrait t -> t `member` cdTraits (getCardDef e)

-- TODO: Chase them down looks at removed
gameSelectEnemy :: (HasGame m) => EnemyMatcher -> m (HashSet EnemyId)
gameSelectEnemy m = do
  villains <- inYourAreaOnly (VillainRef . toId) . HashMap.elems =<< getsGame gameVillains
  minions <- inYourAreaOnly (MinionRef . toId) . HashMap.elems =<< getsGame gameMinions
  removedVillains <-
    if includeRemoved
      then HashMap.elems <$> getsGame (view (removedEntitiesL . villainsL))
      else pure []
  removedMinions <-
    if includeRemoved
      then HashMap.elems <$> getsGame (view (removedEntitiesL . minionsL))
      else pure []
  villains' <-
    map (EnemyVillainId . toId)
      <$> filterM (`goVillain` m) (villains <> removedVillains)
  minions' <-
    map (EnemyMinionId . toId)
      <$> filterM (`goMinion` m) (minions <> removedMinions)
  pure $ HashSet.fromList (villains' <> minions')
 where
  includeRemoved = case m of
    DefeatedEnemy _ -> True
    _ -> False
  goVillain e = \case
    DefeatedEnemy m' -> goVillain e m'
    AttackableByAlly aid -> do
      controller <- getsGame (fmap getAllyController . lookup aid . gameAllies)
      maybe (pure False) (goVillain e . AttackableBy . IdentityWithId) controller
    EnemyWithId enemyId -> pure $ case enemyId of
      EnemyVillainId villainId -> toId e == villainId
      EnemyMinionId _ -> False
    AnyEnemy -> pure True
    VillainEnemy -> pure True
    MinionEnemy -> pure False
    UniqueEnemy -> pure $ cdUnique (getCardDef e)
    DamageableEnemy -> pure True
    -- guard keeps the player it is engaged with from attacking "the villain", the
    -- active one (TWC insert p.6); a 'CannotBeAttacked' villain is attacked by no one
    AttackableBy attacker -> do
      active <- member (toId e) <$> gameSelectVillain ActiveVillain
      guarded <- if active then selectAny (MinionWithKeyword Guard <> MinionEngagedWith attacker) else pure False
      untouchable <- elem CannotBeAttacked <$> getModifiers (VillainRef $ toId e)
      barred <- barredFrom attacker e
      pure $ not (guarded || untouchable || barred)
    UndefendedEnemy ->
      pure $ maybe False (not . attackDefended) (villainAttackDetails e)
    AttackingEnemy -> pure $ isJust (villainAttackDetails e)
    EnemyAttackingCharacter cm -> maybe (pure False) (\a -> member (attackCharacter a) <$> gameSelectCharacter cm) (villainAttackDetails e)
    NotEnemy m' -> not <$> goVillain e m'
    EnemyIs def -> pure $ sameCard def (getCardDef e)
    EnemyMatches xs -> allM (goVillain e) xs
    EnemyWithUpgrade u ->
      selectAny $ u <> UpgradeOneOf (map UpgradeWithId . toList . villainUpgrades $ toAttrs e)
    EnemyWithAttachment m' -> not . null . HashSet.intersection (Villain.villainAttachments $ toAttrs e) <$> select m'
    EnemyWithHighestAttack -> (==) <$> Villain.getModifiedAttack (toAttrs e) <*> highestAttack
    StunnedEnemy -> pure $ Villain.villainStunned (toAttrs e)
    EnemyWithTrait t -> pure $ t `member` cdTraits (getCardDef e)
    ConfusedEnemy -> pure $ Villain.villainConfused (toAttrs e)
    EnemyWithLowestAttack -> (==) <$> Villain.getModifiedAttack (toAttrs e) <*> (minimum <$> attacks)
    EnemyWithHighestScheme -> (==) <$> Villain.getModifiedScheme (toAttrs e) <*> (maximum <$> schemes)
    EnemyWithLowestScheme -> (==) <$> Villain.getModifiedScheme (toAttrs e) <*> (minimum <$> schemes)
  goMinion e = \case
    DefeatedEnemy m' -> goMinion e m'
    EnemyWithId enemyId -> pure $ case enemyId of
      EnemyVillainId _ -> False
      EnemyMinionId minionId -> toId e == minionId
    AnyEnemy -> pure True
    VillainEnemy -> pure False
    MinionEnemy -> pure True
    UniqueEnemy -> pure $ cdUnique (getCardDef e)
    DamageableEnemy -> pure True
    AttackableBy attacker -> not <$> barredFrom attacker e
    AttackableByAlly aid -> do
      allowed <- notElem CannotAttackMinions <$> getModifiers (AllyRef aid)
      controller <- getsGame (fmap getAllyController . lookup aid . gameAllies)
      maybe (pure False) (\c -> (allowed &&) <$> goMinion e (AttackableBy $ IdentityWithId c)) controller
    UndefendedEnemy ->
      pure $ maybe False (not . attackDefended) (minionAttackDetails e)
    AttackingEnemy -> pure $ isJust (minionAttackDetails e)
    EnemyAttackingCharacter cm -> maybe (pure False) (\a -> member (attackCharacter a) <$> gameSelectCharacter cm) (minionAttackDetails e)
    NotEnemy m' -> not <$> goMinion e m'
    EnemyIs def -> pure $ sameCard def (getCardDef e)
    EnemyMatches xs -> allM (goMinion e) xs
    EnemyWithUpgrade u ->
      selectAny $ u <> UpgradeOneOf (map UpgradeWithId . toList . minionUpgrades $ toAttrs e)
    EnemyWithAttachment m' -> not . null . HashSet.intersection (minionAttachments $ toAttrs e) <$> select m'
    EnemyWithHighestAttack -> (==) <$> Minion.getModifiedAttack (toAttrs e) <*> highestAttack
    StunnedEnemy -> pure $ minionStunned (toAttrs e)
    EnemyWithTrait t -> member t <$> getTraits e
    ConfusedEnemy -> pure $ minionConfused (toAttrs e)
    EnemyWithLowestAttack -> (==) <$> Minion.getModifiedAttack (toAttrs e) <*> (minimum <$> attacks)
    EnemyWithHighestScheme -> (==) <$> Minion.getModifiedScheme (toAttrs e) <*> (maximum <$> schemes)
    EnemyWithLowestScheme -> (==) <$> Minion.getModifiedScheme (toAttrs e) <*> (minimum <$> schemes)

  -- a player kept from attacking the enemy's title (Fear of Kang)
  barredFrom attacker e = do
    mods <- concatMapM getModifiers =<< selectList attacker
    pure $ title (cdName $ getCardDef e) `elem` [t | CannotAttackTitled t <- mods]
  highestAttack = do
    villains <- traverse (Villain.getModifiedAttack . toAttrs) . HashMap.elems =<< getsGame gameVillains
    minions <- traverse (Minion.getModifiedAttack . toAttrs) . HashMap.elems =<< getsGame gameMinions
    pure $ foldl' max 0 (villains <> minions)
  -- every enemy's ATK, and SCH: an enemy is one of them, so neither is empty when asked
  attacks = do
    villains <- traverse (Villain.getModifiedAttack . toAttrs) . HashMap.elems =<< getsGame gameVillains
    minions <- traverse (Minion.getModifiedAttack . toAttrs) . HashMap.elems =<< getsGame gameMinions
    pure $ villains <> minions
  schemes = do
    villains <- traverse (Villain.getModifiedScheme . toAttrs) . HashMap.elems =<< getsGame gameVillains
    minions <- traverse (Minion.getModifiedScheme . toAttrs) . HashMap.elems =<< getsGame gameMinions
    pure $ villains <> minions


gameSelectVillain :: (HasGame m) => VillainMatcher -> m (HashSet VillainId)
gameSelectVillain m = do
  villains <- inYourAreaOnly (VillainRef . toId) . HashMap.elems =<< getsGame gameVillains
  -- removedVillains <- toList <$> getsGame (view (removedEntitiesL . villainsL))
  -- result <- filterM (matchFilter m) (villains <> removedVillains)
  result <- filterM (matchFilter m) villains
  pure $ HashSet.fromList $ map toId result
 where
  matchFilter x = case x of
    -- "the villain": the active villain of "your" area
    ActiveVillain -> \v -> do
      aid <- areaId <$> getYourArea
      getsGame \g -> activeVillainIn g aid == Just (toId v)
    AnyVillain -> pure . const True
    VillainWithId ident' -> pure . (== ident') . toId
    VillainWithDamage gameValueMatcher ->
      gameValueMatches gameValueMatcher . villainDamage
    VillainWithToughStatus -> pure . villainIsTough
    VillainWithTrait trait -> fmap (member trait) . getTraits
    VillainMatches xs -> andM . traverse matchFilter xs

gameSelectMinion :: (HasGame m) => MinionMatcher -> m (HashSet MinionId)
gameSelectMinion m = do
  minions <- inYourAreaOnly (MinionRef . toId) . HashMap.elems =<< getsGame gameMinions
  removedMinions <- case m of
    LeftPlayMinion _ -> HashMap.elems <$> getsGame (view (removedEntitiesL . minionsL))
    _ -> pure []
  result <- filterM (matchFilter m) (minions <> removedMinions)
  pure $ HashSet.fromList $ map toId result
 where
  matchFilter x = case x of
    AnyMinion -> pure . const True
    LeftPlayMinion m' -> matchFilter m'
    MinionWithId ident' -> pure . (== ident') . toId
    MinionWithTrait trait -> fmap (member trait) . getTraits
    MinionWithDamage gameValueMatcher ->
      gameValueMatches gameValueMatcher . getMinionDamage
    MinionEngagedWith identityMatcher -> \minion -> do
      identities <- select identityMatcher
      pure $ getMinionEngagedIdentity minion `member` identities
    MinionIs cardDef -> pure . sameCard cardDef . getCardDef
    MinionWithHighestPrintedHitPoints -> \minion -> do
      minions <- HashMap.elems <$> getsGame gameMinions
      let highest = maximum $ map (unHp . getMinionPrintedHitPoints) minions
      pure . (== highest) . unHp $ getMinionPrintedHitPoints minion
    -- a keyword it gains counts, as guard from Defensive Programming
    MinionWithKeyword k -> fmap (elem k) . Minion.getModifiedKeywords . toAttrs
    StunnedMinion -> pure . minionStunned . toAttrs
    MinionMatches xs -> andM . traverse matchFilter xs
    NotMinion m' -> fmap not . matchFilter m'
    MinionWithAttachment attachmentMatcher -> \minion -> do
      attachments <- select attachmentMatcher
      pure . not . null $ minionAttachments (toAttrs minion) `HashSet.intersection` attachments
    MinionWithHighestPrintedHitPointsAmong m' -> \minion -> do
      minions <- HashMap.elems <$> getsGame gameMinions
      candidates <- filterM (matchFilter m') minions
      let highest = maximum $ map (unHp . getMinionPrintedHitPoints) candidates
      pure $
        toId minion `elem` map toId candidates
          && unHp (getMinionPrintedHitPoints minion) == highest
    MinionWithMostRemainingHitPointsAmong m' -> \minion -> do
      minions <- HashMap.elems <$> getsGame gameMinions
      candidates <- filterM (matchFilter m') minions
      let remaining mn = subtractNatural (getMinionDamage mn) <$> Minion.getModifiedHitPoints (toAttrs mn)
      most <- maximum . (0 :) <$> traverse remaining candidates
      mine <- remaining minion
      pure $ toId minion `elem` map toId candidates && mine == most

gameSelectTreachery :: (HasGame m) => TreacheryMatcher -> m (HashSet TreacheryId)
gameSelectTreachery = \case
  AnyTreachery -> do
    treacheries <- HashMap.elems <$> getsGame gameTreacheries
    pure $ HashSet.fromList $ map toId treacheries

gameSelectScheme :: (HasGame m) => SchemeMatcher -> m (HashSet SchemeId)
gameSelectScheme m = fmap HashSet.fromList . inYourAreaOnly SchemeRef . HashSet.toList =<< gameSelectAnyScheme m

-- | The schemes matching, in any area.
gameSelectAnyScheme :: (HasGame m) => SchemeMatcher -> m (HashSet SchemeId)
gameSelectAnyScheme = \case
  AnyScheme -> do
    mainSchemes <- HashMap.elems <$> getsGame gameMainSchemes
    sideSchemes <- HashMap.elems <$> getsGame gameSideSchemes
    pure $
      HashSet.fromList $
        map (SchemeMainSchemeId . toId) mainSchemes
          <> map (SchemeSideSchemeId . toId) sideSchemes
  Matchers.MainScheme -> do
    mainSchemeIds <-
      map (SchemeMainSchemeId . toId) . HashMap.elems <$> getsGame gameMainSchemes
    pure $ HashSet.fromList mainSchemeIds
  SchemeWithId enemyId -> case enemyId of
    SchemeMainSchemeId mainSchemeId -> do
      mainSchemes <- HashMap.elems <$> getsGame gameMainSchemes
      pure . fromList $
        map (SchemeMainSchemeId . toId) $
          filter
            ((== mainSchemeId) . toId)
            mainSchemes
    SchemeSideSchemeId sideSchemeId ->
      HashSet.map SchemeSideSchemeId
        <$> gameSelectSideScheme (SideSchemeWithId sideSchemeId)
  ThwartableScheme -> thwartable =<< selectAny CrisisSideScheme
  ThwartableSchemeIgnoringCrisis -> thwartable False
  SchemeYouCanThwart -> do
    schemes <- gameSelectAnyScheme ThwartableScheme
    patrolled <- isPatrolled =<< getYou
    pure $ if patrolled then HashSet.filter (not . isMain) schemes else schemes
  UnpatrolledMainScheme -> do
    patrolled <- isPatrolled =<< getYou
    if patrolled then pure mempty else gameSelectAnyScheme Matchers.MainScheme
 where
  isMain = \case
    SchemeMainSchemeId _ -> True
    _ -> False
  thwartable crisisSideSchemes = do
    you <- getYou
    let removable = \case
          ThreatCannotBeRemoved -> False
          ThreatRemovableOnlyBy p -> p == you
          _ -> True
    sideSchemes <- filterM (fmap (all removable) . getModifiers) . HashMap.elems =<< getsGame gameSideSchemes
    if crisisSideSchemes
      then
        pure $
          HashSet.fromList $
            map
              (SchemeSideSchemeId . toId)
              (filter ((> 0) . getSideSchemeThreat) sideSchemes)
      else do
        mainSchemes <- filterM (fmap (all removable) . getModifiers) . HashMap.elems =<< getsGame gameMainSchemes
        pure $
          HashSet.fromList $
            map
              (SchemeMainSchemeId . toId)
              (filter ((> 0) . getMainSchemeThreat) mainSchemes)
              <> map
                (SchemeSideSchemeId . toId)
                (filter ((> 0) . getSideSchemeThreat) sideSchemes)

gameSelectCountScheme ::
  (HasGame m) => QueryCount SchemeMatcher -> SchemeMatcher -> m Natural
gameSelectCountScheme aggregate matcher = do
  schemes <- toList <$> gameSelectScheme matcher
  case aggregate of
    SchemeThreat -> do
      let
        toThreat = \case
          SchemeMainSchemeId sid -> do
            mMainScheme <- getsGame (lookup sid . gameMainSchemes)
            pure $ maybe 0 getMainSchemeThreat mMainScheme
          SchemeSideSchemeId sid -> do
            mSideScheme <- getsGame (lookup sid . gameSideSchemes)
            pure $ maybe 0 getSideSchemeThreat mSideScheme
      sum <$> traverse toThreat schemes

gameSelectSideScheme ::
  (HasGame m) => SideSchemeMatcher -> m (HashSet SideSchemeId)
gameSelectSideScheme m = do
  sideSchemes <- inYourAreaOnly (SideSchemeRef . toId) . HashMap.elems =<< getsGame gameSideSchemes
  removedSideSchemes <- case m of
    LeftPlaySideScheme _ -> HashMap.elems <$> getsGame (view (removedEntitiesL . sideSchemesL))
    _ -> pure []
  result <- filterM (matchFilter m) (sideSchemes <> removedSideSchemes)
  pure $ HashSet.fromList $ map toId result
 where
  matchFilter = \case
    AnySideScheme -> pure . const True
    LeftPlaySideScheme m' -> matchFilter m'
    CrisisSideScheme -> pure . isCrisis
    SideSchemeIs def -> pure . sameCard def . getCardDef
    SideSchemeWithId ident -> pure . (== ident) . toId
    SideSchemeWithUpgrade upgradeMatcher -> \sideScheme -> do
      upgrades <- select upgradeMatcher
      attached <- getsGame $ filter ((== Just (toId sideScheme)) . upgradeAttachedSideScheme . toAttrs) . HashMap.elems . gameUpgrades
      pure $ any ((`member` upgrades) . toId) attached
    NotSideScheme m' -> fmap not . matchFilter m'
    SignatureSchemeOf villainMatcher -> \sideScheme -> do
      villains <- select villainMatcher
      getsGame \g -> any (`member` villains) [v | (v, sid) <- HashMap.toList (gameSignatureSchemes g), sid == toId sideScheme]

withDepthGuard :: (HasGame m, Monoid a) => Int -> ReaderT Game m a -> m a
withDepthGuard depthGuard body = do
  depth <- getsGame gameDepth
  if depth > depthGuard
    then pure mempty
    else getGame >>= runReaderT (local delve body)

delve :: Game -> Game
delve g = g {gameDepth = gameDepth g + 1}

-- | The cards held by the upgrades the player controls, played as if in hand.
getUpgradeHeldCards :: (HasGame m) => IdentityId -> m [PlayerCard]
getUpgradeHeldCards ident =
  getsGame \g -> [c | u <- HashMap.elems (gameUpgrades g), getUpgradeController u == ident, c <- upgradeHeldCards (toAttrs u)]

getModifiers :: (HasGame m, IsRef a) => a -> m [Modifier]
getModifiers a = withDepthGuard 2 $ do
  effects <- HashMap.elems <$> getsGame gameEffects
  -- a card whose text box is blank gives no modifiers
  upgrades <- filterM (fmap not . isBlanked) . HashMap.elems =<< getsGame gameUpgrades
  supports <- filterM (fmap not . isBlanked) . HashMap.elems =<< getsGame gameSupports
  attachments <- HashMap.elems <$> getsGame gameAttachments
  allies <- filterM (fmap not . isBlanked) . HashMap.elems =<< getsGame gameAllies
  minions <- HashMap.elems <$> getsGame gameMinions
  events <- HashMap.elems <$> getsGame gameEvents
  players <- HashMap.elems <$> getsGame gamePlayers
  sideSchemes <- HashMap.elems <$> getsGame gameSideSchemes
  mainSchemes <- HashMap.elems <$> getsGame gameMainSchemes
  villains <- HashMap.elems <$> getsGame gameVillains
  environments <- HashMap.elems <$> getsGame gameEnvironments
  treacheries <- HashMap.elems <$> getsGame gameTreacheries
  obligations <- HashMap.elems <$> getsGame gameObligations
  -- a card in hand answers for itself: "reduce the cost to play this card". ponytail:
  -- allies and events only, the kinds so far that do
  let inHandOf :: (HasGame m) => CardType -> (IdentityId -> PlayerCard -> x) -> m [x]
      inHandOf cardType create = case toTarget a of
        CardIdRef cid -> getsGame \g ->
          [ create (toId p) c
          | p <- HashMap.elems (gamePlayers g)
          , c <- unHand (playerIdentityHand $ toAttrs p)
          , pcCardId c == cid
          , cdCardType (getCardDef c) == cardType
          ]
        _ -> pure []
  inHand <- inHandOf AllyType createAlly
  eventsInHand <- inHandOf EventType createEvent
  mconcat
    <$> sequence
      [ concatMapM (getModifiersFor (toSource a) (toTarget a)) effects
      , concatMapM (getModifiersFor (toSource a) (toTarget a)) inHand
      , concatMapM (getModifiersFor (toSource a) (toTarget a)) eventsInHand
      , concatMapM (getModifiersFor (toSource a) (toTarget a)) upgrades
      , concatMapM (getModifiersFor (toSource a) (toTarget a)) supports
      , concatMapM (getModifiersFor (toSource a) (toTarget a)) attachments
      , concatMapM (getModifiersFor (toSource a) (toTarget a)) allies
      , concatMapM (getModifiersFor (toSource a) (toTarget a)) minions
      , concatMapM (getModifiersFor (toSource a) (toTarget a)) events
      , concatMapM (getModifiersFor (toSource a) (toTarget a)) players
      , concatMapM (getModifiersFor (toSource a) (toTarget a)) sideSchemes
      , concatMapM (getModifiersFor (toSource a) (toTarget a)) mainSchemes
      , concatMapM (getModifiersFor (toSource a) (toTarget a)) villains
      , concatMapM (getModifiersFor (toSource a) (toTarget a)) environments
      , concatMapM (getModifiersFor (toSource a) (toTarget a)) treacheries
      , concatMapM (getModifiersFor (toSource a) (toTarget a)) obligations
      ]

getCurrentWindows :: (HasGame m) => m [Window]
getCurrentWindows = do
  windows <- getsGame gameWindows
  pure $ case windows of
    [] -> []
    x : _ -> x

getActiveCost :: (HasGame m) => m (Maybe ActiveCostId)
getActiveCost = do
  costStack <- getsGame gameActiveCost
  pure $ case costStack of
    [] -> Nothing
    cost : _ -> Just $ activeCostId cost

getCurrentPayment :: (HasGame m) => m Payment
getCurrentPayment = do
  costStack <- getsGame gameActiveCost
  pure $ case costStack of
    [] -> NoPayment
    cost : _ -> activeCostPayment cost

getDifficulty :: (HasGame m) => m Difficulty
getDifficulty = getsGame $ getScenarioDifficulty . gameScenario

abilityResources ::
  (HasQueue m, MonadThrow m, HasGame m) => Ability -> m [Resource]
abilityResources a = go (abilityChoices a)
 where
  go [] = pure []
  go (Pay (ResourcePayment r) : xs) = (r :) <$> go xs
  go (Pay (ResourcePaymentFromCard matcher) : xs) = do
    cards <- selectList matcher
    case cards of
      [] -> go xs
      [c] -> (<>) (printedResources $ getCardDef c) <$> go xs
      _ -> error "invalid matcher"
  go (_ : xs) = go xs

-- | The hazard icons in "your" area.
getHazardCount :: (HasGame m) => m Natural
getHazardCount = getHazardCountIn . areaId =<< getYourArea

getHazardCountIn :: (HasGame m) => AreaId -> m Natural
getHazardCountIn aid = getsGame \g ->
  sum (map (cdHazards . getCardDef) (inArea g aid SideSchemeRef (gameSideSchemes g)))
    + sum (map (cdHazards . getCardDef) (inArea g aid VillainRef (gameVillains g)))
    + sum (map (cdHazards . getCardDef) (inArea g aid MainSchemeRef (gameMainSchemes g)))

-- | The cards in the victory display, the first added first (RR Victory Display).
getVictoryDisplay :: (HasGame m) => m [EncounterCard]
getVictoryDisplay = getsGame gameVictoryDisplay

-- | The cards in The Collection (Infiltrate the Museum), the first added first: a player
-- card's owner is its 'pcOwner'.
getCollection :: (HasGame m) => m [Card]
getCollection = getsGame gameCollection

-- | The victory points in the victory display: each card's X (RR Victory X).
getVictoryPoints :: (HasGame m) => m Natural
getVictoryPoints = sum . map victoryValue <$> getVictoryDisplay

-- | A card's X, for victory X, if it has the keyword.
victoryValue :: (HasCardDef a) => a -> Natural
victoryValue a = sum [n | Victory n <- toList (cdKeywords $ getCardDef a)]

-- | Indirect damage (RR): assigned a point at a time, by the player given, among the
-- characters the players given control that can take damage, none past what would defeat
-- it (a tough one too, its card preventing what it is given), then dealt at once; what
-- cannot be assigned is ignored. The step given goes on with what is left to assign.
assignIndirectDamage ::
  (ReverseQueue m) =>
  IdentityId ->
  [IdentityId] ->
  Source ->
  Damage ->
  [(CharacterId, Natural)] ->
  (Damage -> [(CharacterId, Natural)] -> m ()) ->
  m ()
assignIndirectDamage chooser idents source damage assigned next = do
  g <- getGame
  let n = damageAmount damage
      given c = sum [k | (c', k) <- assigned, c' == c]
  identity <- for [(i, p) | i <- idents, Just p <- [lookup i (gamePlayers g)]] \(i, p) -> (IdentityCharacter i,,) <$> getModifiedHp p <*> pure (identityDamage p)
  allies <- for [a | a <- HashMap.elems (gameAllies g), getAllyController a `elem` idents] \a ->
    (AllyCharacter (toId a),,) <$> Ally.getModifiedHitPoints (toAttrs a) <*> pure (getAllyDamage a)
  open <-
    filterM (fmap (notElem CannotTakeDamage) . getModifiers . characterRef . fst)
      [(c, room) | (c, hp, damage) <- identity <> allies, let room = subtractNatural (damage + given c) hp, room > 0]
  let assign c k = next damage {damageAmount = n - k} ((c, k) : assigned)
  case open of
    _ | n == 0 || null open ->
      pushAll . simultaneousDamage =<< traverse (\(c, k) -> capture $ damageCharacter c source damage {damageAmount = k}) (HashMap.toList $ HashMap.fromListWith (+) assigned)
    [(c, room)] -> assign c (min n room)
    _ -> chooseOnePrompted chooser "$label.assignIndirectDamage" [targeting (characterRef c) (assign c 1) | (c, _) <- open]

-- * The Milano and the Power Stone (The Galaxy's Most Wanted)

-- | The Milano support (Ship Command), while it is in play.
getMilano :: (HasGame m) => m (Maybe SupportId)
getMilano = getsGame \g -> headMay [toId s | s <- HashMap.elems (gameSupports g), isMilano s]

isMilano :: Support -> Bool
isMilano = (== "Milano") . title . cdName . supportCardDef . toAttrs

-- | Whether the Milano is in play and ready, so it can be exhausted: "exhaust the Milano"
-- is a cost and an option only then.
milanoIsReady :: (HasGame m) => m Bool
milanoIsReady = getsGame \g -> any (\s -> isMilano s && not (isExhausted s)) (HashMap.elems $ gameSupports g)

-- | Whether the player controls the Milano: the first player does.
controlsMilano :: (HasGame m) => IdentityId -> m Bool
controlsMilano ident = getsGame \g -> any (\s -> isMilano s && getSupportController s == ident) (HashMap.elems $ gameSupports g)

-- | The Power Stone attachment (Power Stone), while it is in play.
getPowerStone :: (HasGame m) => m (Maybe AttachmentId)
getPowerStone = getsGame \g -> headMay [toId a | a <- HashMap.elems (gameAttachments g), title (cdName $ getCardDef a) == "Power Stone"]

-- | The character an attachment is attached to: an enemy or an identity.
getAttachedCharacter :: (HasGame m) => AttachmentId -> m (Maybe CharacterId)
getAttachedCharacter aid = getsGame \g -> do
  at <- toAttrs <$> lookup aid (gameAttachments g)
  (enemyCharacter <$> Attachment.attachmentEnemy at) <|> (IdentityCharacter <$> Attachment.attachmentIdentity at)
 where
  enemyCharacter = \case
    EnemyVillainId v -> VillainCharacter v
    EnemyMinionId m -> MinionCharacter m

-- | The hero or villain the Power Stone is attached to.
getPowerStoneHolder :: (HasGame m) => m (Maybe CharacterId)
getPowerStoneHolder = maybe (pure Nothing) getAttachedCharacter =<< getPowerStone

-- | Whether the player controls the Power Stone: it is attached to their identity.
controlsPowerStone :: (HasGame m) => IdentityId -> m Bool
controlsPowerStone ident = (== Just (IdentityCharacter ident)) <$> getPowerStoneHolder

-- | The amplify icons in play in "your" area (RR Amplify Icon): on its schemes, villains,
-- minions, attachments and environments.
getAmplifyCount :: (HasGame m) => m Natural
getAmplifyCount = do
  aid <- areaId <$> getYourArea
  getsGame \g ->
    let icons :: (HasCardDef a) => (k -> Target) -> HashMap k a -> Natural
        icons ref = sum . map (cdAmplify . getCardDef) . inArea g aid ref
     in icons SideSchemeRef (gameSideSchemes g)
          + icons MainSchemeRef (gameMainSchemes g)
          + icons VillainRef (gameVillains g)
          + icons MinionRef (gameMinions g)
          + icons AttachmentRef (gameAttachments g)
          + icons EnvironmentRef (gameEnvironments g)

-- | The cards of the map in the area.
inArea :: Game -> AreaId -> (k -> Target) -> HashMap k a -> [a]
inArea g aid ref = map snd . filter (\(k, _) -> (areaId <$> areaOfTarget g (ref k)) == Just aid) . HashMap.toList

-- | The acceleration tokens and the acceleration icons in "your" area.
getAccelerationCount :: (HasGame m) => m Natural
getAccelerationCount = getAccelerationCountIn . areaId =<< getYourArea

-- | The acceleration tokens next to the area's main scheme, which stay as it leaves play
-- (RR, Acceleration Token), and the area's acceleration icons.
getAccelerationCountIn :: (HasGame m) => AreaId -> m Natural
getAccelerationCountIn aid = getsGame \g ->
  sum [areaAccelerationTokens a | a <- gameAreas g, areaId a == aid] + accelerationIconsIn g aid

-- | The acceleration icons in the area, on its schemes, villains and obligations.
accelerationIconsIn :: Game -> AreaId -> Natural
accelerationIconsIn g aid =
  sum (map (cdAcceleration . getCardDef) (inArea g aid SideSchemeRef (gameSideSchemes g)))
    + sum (map (cdAcceleration . getCardDef) (inArea g aid VillainRef (gameVillains g)))
    -- an obligation in a player's play area, in theirs
    + sum
      [ cdAcceleration (getCardDef o)
      | o <- HashMap.elems (gameObligations g)
      , Just owner <- [obligationOwner (toAttrs o)]
      , (areaId <$> areaOfTarget g (IdentityRef owner)) == Just aid
      ]
    + sum (map (cdAcceleration . getCardDef) (inArea g aid MainSchemeRef (gameMainSchemes g)))

gameSelectCountIdentity ::
  (HasGame m) => QueryCount IdentityMatcher -> IdentityMatcher -> m Natural
gameSelectCountIdentity aggregate matcher = do
  identities <-
    filterM (identityMatches matcher . toId) . HashMap.elems =<< getsGame gamePlayers
  case aggregate of
    SustainedDamage -> pure . sum $ map identityDamage identities
    HeroAttackDamage -> sum <$> traverse getIdentityHeroAttackDamage identities
    HeroThwartValue -> sum <$> traverse getIdentityHeroThwart identities

-- | The allies' modified ATK or THW, summed.
gameSelectCountAlly ::
  (HasGame m) => QueryCount AllyMatcher -> AllyMatcher -> m Natural
gameSelectCountAlly aggregate matcher = do
  allies <- filterM (allyMatches matcher . toId) . HashMap.elems =<< getsGame gameAllies
  sum <$> for allies \ally -> case aggregate of
    AllyAttackValue -> Ally.getModifiedAttack (toAttrs ally)
    AllyThwartValue -> Ally.getModifiedThwart (toAttrs ally)
