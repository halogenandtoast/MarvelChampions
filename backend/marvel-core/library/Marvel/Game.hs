{-# LANGUAGE TemplateHaskell #-}
module Marvel.Game where

import Marvel.Prelude

import qualified Data.Aeson.Diff as Diff
import qualified Data.HashSet as HashSet
import Marvel.Ability
import Marvel.Ally
import Marvel.AlterEgo.Cards
import Marvel.Card.Code
import Marvel.Card.Def
import Marvel.Card.Id
import Marvel.Card.PlayerCard
import Marvel.Debug
import Marvel.Deck
import Marvel.Entity
import Marvel.Event
import Marvel.Exception
import Marvel.Hand
import Marvel.Hero.Cards
import Marvel.Id
import Marvel.Identity hiding (alliesL, supportsL)
import Marvel.Matchers
import Marvel.Message
import Marvel.Minion
import Marvel.Phase
import Marvel.Query
import Marvel.Question
import Marvel.Queue
import Marvel.Resource
import Marvel.Scenario
import Marvel.Scenario.Attrs (scenarioId)
import Marvel.Support
import Marvel.Target
import Marvel.Villain
import Marvel.Window

data GameState = Unstarted | InProgress | Finished
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON)

type EntityMap a = HashMap (EntityId a) a

data Game = Game
  { gamePhase :: Phase
  , gameState :: GameState
  , -- players in player order
    gamePlayerOrder :: [IdentityId]
  , gamePlayers :: EntityMap PlayerIdentity
  , gameVillains :: EntityMap Villain
  , gameMinions :: EntityMap Minion
  , gameAllies :: EntityMap Ally
  , gameSupports :: EntityMap Support
  , gameEvents :: EntityMap Event
  , gameQuestion :: HashMap IdentityId Question
  , gameUsedAbilities :: HashMap IdentityId [Ability]
  , gameActivePlayer :: IdentityId
  , gameActiveCost :: Maybe ActiveCost
  , gameWindowDepth :: Int
  , gameScenario :: Scenario
  }
  deriving stock (Show, Eq, Generic)

makeLensesWith suffixedFields ''Game

instance ToJSON Game where
  toJSON = genericToJSON $ aesonOptions $ Just "game"

instance FromJSON Game where
  parseJSON = genericParseJSON $ aesonOptions $ Just "game"

diff :: Game -> Game -> Diff.Patch
diff a b = Diff.diff (toJSON a) (toJSON b)

patch :: Game -> Diff.Patch -> Result Game
patch g p = case Diff.patch p (toJSON g) of
  Error e -> Error e
  Success a -> fromJSON a

getPlayers :: MonadGame env m => m [IdentityId]
getPlayers = getsGame gamePlayerOrder

getPlayerCount :: MonadGame env m => m Int
getPlayerCount = length <$> getsGame gamePlayerOrder

getActivePlayer :: MonadGame env m => m PlayerIdentity
getActivePlayer = getsGame $ \g ->
  case lookup (g ^. activePlayerL) (g ^. playersL) of
    Just p -> p
    Nothing -> error "Missing player"

runPreGameMessage :: MonadGame env m => Message -> Game -> m Game
runPreGameMessage msg g = case msg of
  CheckWindows{} -> do
    push EndCheckWindows
    pure $ g & windowDepthL +~ 1
  -- We want to empty the queue for triggering a resolution
  EndCheckWindows -> pure $ g & windowDepthL -~ 1
  _ -> pure g

runGameMessage :: MonadGame env m => Message -> Game -> m Game
runGameMessage msg g@Game {..} = case msg of
  StartGame -> do
    push StartScenario
    pushAll $ map (`IdentityMessage` SetupIdentity) gamePlayerOrder
    case gamePlayerOrder of
      [] -> throwM NoPlayers
      [p] -> push (SetPlayerOrder [p])
      players@(p : _) -> choosePlayerOrder p players
    pure $ g { gameState = InProgress }
  SetPlayerOrder xs -> pure $ g { gamePlayerOrder = xs }
  RemoveFromPlay target -> case target of
    AllyTarget aid -> do
      for_ (lookup aid gameAllies) $ \ally ->
        push $ IdentityMessage (getAllyController ally) $ AllyRemoved aid
      pure $ g & alliesL %~ delete aid
    SupportTarget sid -> do
      for_ (lookup sid gameSupports) $ \support ->
        push $ IdentityMessage (getSupportController support) $ SupportRemoved sid
      pure $ g & supportsL %~ delete sid
    _ -> error "Unhandled target"
  AddVillain cardCode -> do
    villainId <- getRandom
    case lookupVillain cardCode villainId of
      Just x -> do
        push $ VillainMessage villainId SetVillainHp
        pure $ g & villainsL . at villainId ?~ x
      Nothing -> throwM $ MissingCardCode "AddVillain" cardCode
  UsedAbility ident a -> pure $ g & usedAbilitiesL %~ insertWith (<>) ident [a]
  SetActiveCost activeCost -> do
    cards <- getAvailablePaymentSources
    abilities <- getResourceAbilities
    push $ Ask
      (activeCostIdentityId activeCost)
      (ChooseOne
      $ map PayWithCard cards
      <> map UseAbility abilities
      <> [ FinishPayment | resourceCostPaid activeCost ]
      )
    pure $ g & activeCostL ?~ activeCost
  Spent discard -> case g ^. activeCostL of
    Just activeCost -> do
      case activeCostTarget activeCost of
        ForCard card -> do
          push $ Paid $ mconcat $ map
            ResourcePayment
            (resourcesFor discard card)
          pure g
    Nothing -> error "No active cost"
  Paid payment -> case g ^. activeCostL of
    Just activeCost -> do
      let
        activeCost' = activeCost
          { activeCostPayment = activeCostPayment activeCost <> payment
          }
      cards <- getAvailablePaymentSources
      abilities <- getResourceAbilities
      push $ Ask
        (activeCostIdentityId activeCost)
        (ChooseOne
        $ map PayWithCard cards
        <> map UseAbility abilities
        <> [ FinishPayment | resourceCostPaid activeCost' ]
        )
      pure $ g & activeCostL ?~ activeCost'
    Nothing -> error "No cost"
  FinishedPayment -> case g ^. activeCostL of
    Just activeCost -> do
      case activeCostTarget activeCost of
        ForCard card -> do
          push $ PutCardIntoPlay
            (activeCostIdentityId activeCost)
            card
            (activeCostPayment activeCost)
          pure $ g & activeCostL .~ Nothing
    Nothing -> error "no active cost"
  PutCardIntoPlay ident card payment -> do
    case cdCardType (getCardDef card) of
      AllyType -> do
        let ally = createAlly ident card
        pushAll
          [ IdentityMessage ident (AllyCreated $ toId ally)
          , CheckWindows [Window After $ PlayedAlly (toId ally)]
          ]
        pure $ g & alliesL %~ insert (toId ally) ally
      SupportType -> do
        let support = createSupport ident card
        pushAll
          [ IdentityMessage ident (SupportCreated $ toId support)
          , CheckWindows [Window After $ PlayedSupport (toId support)]
          ]
        pure $ g & supportsL %~ insert (toId support) support
      EventType -> do
        let event = createEvent ident card
        push $ EventMessage (toId event) $ PlayedEvent ident payment
        pure $ g & eventsL %~ insert (toId event) event
      _ -> error "Unhandled"
  EndCheckWindows -> pure g
  CheckWindows windows -> do
    abilities <- getsGame getAbilities
    usedAbilities <- getUsedAbilities
    ident <- toId <$> getActivePlayer
    validAbilities <- filterM
      (andM . sequence
        [ pure . passesUseLimit ident usedAbilities
        , passesCriteria ident
        , \a -> pure $ any (`abilityInWindow` a) windows
        ]
      )
      abilities
    let forcedAbiltiies = filter isForcedAbility validAbilities
    case (forcedAbiltiies, validAbilities) of
      ([], []) -> pure ()
      ([], as) ->
        push
          $ Ask ident
          . ChooseOne
          $ Label "Use no responses/interrupts" []
          : map (UseAbility . (choicesL <>~ [Run [CheckWindows windows]])) as
      (forced, _) -> push $ Ask ident . ChooseOne $ map
        (UseAbility . (choicesL <>~ [Run [CheckWindows windows]]))
        forced
    pure g
  DeclareDefense ident enemyId -> do
    identities <- select $ UnexhaustedIdentity <> HeroIdentity
    allies <- selectList UnexhaustedAlly
    push
      $ Ask ident
      . ChooseOne
      $ Label "No defenders" []
      : [ Defend enemyId | ident `member` identities ]
      <> map (`AllyDefend` enemyId) allies
    pure g
  _ -> pure g

abilityInWindow :: Window -> Ability -> Bool
abilityInWindow window a = maybe
  False
  (\matcher -> windowMatches matcher window source)
  (abilityWindow a)
  where source = abilitySource a

instance RunMessage Game where
  runMessage msg g =
    runPreGameMessage msg g
      >>= traverseOf scenarioL (runMessage msg)
      >>= traverseOf (playersL . each) (runMessage msg)
      >>= traverseOf (alliesL . each) (runMessage msg)
      >>= traverseOf (villainsL . each) (runMessage msg)
      >>= traverseOf (eventsL . each) (runMessage msg)
      >>= runGameMessage msg

class HasGame a where
  game :: a -> IORef Game

createAlly :: IdentityId -> PlayerCard -> Ally
createAlly ident card =
  lookupAlly (toCardCode card) ident (AllyId $ unCardId $ pcCardId card)

createSupport :: IdentityId -> PlayerCard -> Support
createSupport ident card =
  lookupSupport (toCardCode card) ident (SupportId $ unCardId $ pcCardId card)

createEvent :: IdentityId -> PlayerCard -> Event
createEvent ident card =
  lookupEvent (toCardCode card) ident (EventId $ unCardId $ pcCardId card)

newGame :: PlayerIdentity -> Scenario -> Game
newGame player scenario = Game
  { gamePhase = PlayerPhase
  , gameState = Unstarted
  , gamePlayerOrder = [toId player]
  , gamePlayers = fromList [(toId player, player)]
  , gameVillains = mempty
  , gameMinions = mempty
  , gameQuestion = mempty
  , gameActiveCost = Nothing
  , gameWindowDepth = 0
  , gameUsedAbilities = mempty
  , gameActivePlayer = toId player
  , gameScenario = scenario
  , gameAllies = mempty
  , gameSupports = mempty
  , gameEvents = mempty
  }

addPlayer :: MonadGame env m => PlayerIdentity -> m ()
addPlayer player =
  withGame_
    $ (playersL %~ insert (toId player) player)
    . (playerOrderL <>~ [toId player])

withGame :: MonadGame env m => (Game -> (Game, a)) -> m a
withGame f = do
  gameRef <- asks game
  atomicModifyIORef' gameRef f

withGame_ :: MonadGame env m => (Game -> Game) -> m ()
withGame_ f = withGame ((, ()) . f)

withGameM :: MonadGame env m => (Game -> m Game) -> m ()
withGameM f = getGame >>= f >>= withGame_ . const

getsGame :: MonadGame env m => (Game -> a) -> m a
getsGame f = withGame (id &&& f)

getUsedAbilities :: MonadGame env m => m (HashMap IdentityId [Ability])
getUsedAbilities = getsGame (view usedAbilitiesL)

class
  ( MonadCatch m
  , MonadThrow m
  , MonadIO m
  , MonadReader env m
  , HasGame env
  , HasQueue env
  , HasDebugLogger env
  , MonadRandom m
  )
  => MonadGame env m | env -> m, m -> env

initPlayer :: MonadRandom m => CardCode -> Deck -> m PlayerIdentity
initPlayer cardCode deck = do
  ident <- getRandom
  let
    mAlterEgo = do
      def <- lookup (toAlterEgoCardCode cardCode) allAlterEgosMap
      lookupAlterEgo def ident
    mHero = do
      def <- lookup (toHeroCardCode cardCode) allHeroesMap
      lookupHero def ident
  case (mAlterEgo, mHero) of
    (Just alterEgoSide, Just heroSide) ->
      pure . setDeck deck $ createIdentity ident alterEgoSide heroSide
    _ -> error "stuff"

createPlayer :: MonadGame env m => CardCode -> Deck -> m ()
createPlayer cardCode deck = do
  playerIdentity <- initPlayer cardCode deck
  addPlayer playerIdentity

getGame :: MonadGame env m => m Game
getGame = readIORef =<< asks game

instance HasAbilities Game where
  getAbilities g = concatMap getAbilities (elems $ gamePlayers g)
    <> concatMap getAbilities (elems $ gameAllies g)

runGameMessages :: (MonadGame env m, CoerceRole m) => m ()
runGameMessages = do
  mMsg <- pop
  for_ mMsg debug
  for_ mMsg $ \case
    Ask ident choices -> do
      withGame_ $ questionL .~ fromList [(ident, choices)]
    other -> do
      withGame_ $ questionL .~ mempty
      withGameM $ runMessage other
      runGameMessages

-- TODO: implement this for api
replayChoices :: MonadGame env m => [Diff.Patch] -> m ()
replayChoices _ = pure ()

getAvailablePaymentSources :: MonadGame env m => m [PlayerCard]
getAvailablePaymentSources = do
  players <- toList <$> getsGame gamePlayers
  pure $ concatMap (unHand . playerIdentityHand) players

getAvailableResourcesFor :: MonadGame env m => PlayerCard -> m [Resource]
getAvailableResourcesFor c = do
  players <- toList <$> getsGame gamePlayers
  pure $ concatMap (`resourcesFor` c) players

getResourceAbilities :: MonadGame env m => m [Ability]
getResourceAbilities = do
  player <- getActivePlayer
  abilities <- getsGame getAbilities
  usedAbilities <- getUsedAbilities
  pure $ filter
    (and . sequence
      [passesUseLimit (toId player) usedAbilities, (== Resource) . abilityType]
    )
    abilities

gameSelectIdentity
  :: MonadGame env m => IdentityMatcher -> m (HashSet IdentityId)
gameSelectIdentity m = do
  identities <- toList <$> getsGame gamePlayers
  pure $ HashSet.fromList $ map toId $ filter (matchFilter m) identities
 where
  matchFilter x = case x of
    AnyIdentity -> const True
    HeroIdentity -> isHero
    UnexhaustedIdentity -> not . isExhausted
    IdentityMatchAll xs -> and . traverse matchFilter xs

gameSelectAlly :: MonadGame env m => AllyMatcher -> m (HashSet AllyId)
gameSelectAlly = \case
  UnexhaustedAlly -> do
    allies <- toList <$> getsGame gameAllies
    pure $ HashSet.fromList $ map toId $ filter (not . isExhausted) allies

gameSelectEnemy :: MonadGame env m => EnemyMatcher -> m (HashSet EnemyId)
gameSelectEnemy = \case
  AnyEnemy -> do
    villains <- toList <$> getsGame gameVillains
    pure $ HashSet.fromList $ map (EnemyVillainId . toId) villains

gameSelectVillain :: MonadGame env m => VillainMatcher -> m (HashSet VillainId)
gameSelectVillain = \case
  ActiveVillain -> do
    villains <- toList <$> getsGame gameVillains
    pure $ HashSet.fromList $ map toId villains

gameSelectMinion :: MonadGame env m => MinionMatcher -> m (HashSet MinionId)
gameSelectMinion = \case
  AnyMinion -> do
    minions <- toList <$> getsGame gameMinions
    pure $ HashSet.fromList $ map toId minions

gameSelectScheme :: MonadGame env m => SchemeMatcher -> m (HashSet SchemeId)
gameSelectScheme = \case
  AnyScheme -> do
    mainSchemeId <-
      SchemeMainSchemeId . scenarioId . toAttrs <$> getsGame gameScenario
    pure $ HashSet.singleton mainSchemeId
  MainScheme -> do
    mainSchemeId <-
      SchemeMainSchemeId . scenarioId . toAttrs <$> getsGame gameScenario
    pure $ HashSet.singleton mainSchemeId
