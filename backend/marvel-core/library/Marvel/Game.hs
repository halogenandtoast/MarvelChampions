{-# LANGUAGE TemplateHaskell #-}
module Marvel.Game where

import Marvel.Prelude

import qualified Data.Aeson.Diff as Diff
import qualified Data.HashSet as HashSet
import Marvel.Ability
import Marvel.Ally
import Marvel.AlterEgo.Cards
import Marvel.Attachment
import Marvel.Card.Code
import Marvel.Card.Def
import Marvel.Card.EncounterCard
import Marvel.Card.Id
import Marvel.Card.PlayerCard
import Marvel.Criteria
import Marvel.Debug
import Marvel.Deck
import Marvel.Effect
import Marvel.Entity
import Marvel.Event
import Marvel.Exception
import Marvel.Hand
import Marvel.Hero.Cards
import Marvel.Id
import Marvel.Identity hiding (alliesL, minionsL, supportsL, upgradesL)
import Marvel.Matchers
import Marvel.Message hiding (ExhaustedAlly)
import Marvel.Minion
import Marvel.Modifier
import Marvel.Phase
import Marvel.Query
import Marvel.Question
import Marvel.Queue
import Marvel.Resource
import Marvel.Scenario
import Marvel.Scenario.Attrs (scenarioId)
import Marvel.SideScheme
import Marvel.Source
import Marvel.Support
import Marvel.Target
import Marvel.Treachery
import Marvel.Upgrade
import Marvel.Villain
import Marvel.Window (Window, WindowTiming(..), windowMatches)
import qualified Marvel.Window as W

data GameState = Unstarted | InProgress | Finished FinishedStatus
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
  , gameAttachments :: EntityMap Attachment
  , gameSupports :: EntityMap Support
  , gameUpgrades :: EntityMap Upgrade
  , gameTreacheries :: EntityMap Treachery
  , gameSideSchemes :: EntityMap SideScheme
  , gameEffects :: EntityMap Effect
  , gameEvents :: EntityMap Event
  , gameQuestion :: HashMap IdentityId Question
  , gameUsedAbilities :: HashMap IdentityId [Ability]
  , gameActivePlayer :: IdentityId
  , gameActiveCost :: Maybe ActiveCost
  , gameWindowDepth :: Int
  , gameWindows :: [[Window]]
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
  CheckWindows windows -> do
    push EndCheckWindows
    pure $ g & windowDepthL +~ 1 & windowsL %~ (windows :)
  -- We want to empty the queue for triggering a resolution
  EndCheckWindows -> pure $ g & windowDepthL -~ 1 & windowsL %~ drop 1
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
        push $ IdentityMessage (getSupportController support) $ SupportRemoved
          sid
      pure $ g & supportsL %~ delete sid
    UpgradeTarget uid -> do
      for_ (lookup uid gameUpgrades) $ \upgrade -> pushAll
        [ UpgradeRemoved uid
        , IdentityMessage
          (getUpgradeController upgrade)
          (DiscardCard $ PlayerCard
            (CardId . unUpgradeId $ toId upgrade)
            (getCardDef upgrade)
            (Just $ getUpgradeController upgrade)
            (Just $ getUpgradeController upgrade)
          )
        ]
      pure $ g & upgradesL %~ delete uid
    MinionTarget mid -> do
      for_ (lookup mid gameMinions) $ \minion ->
        push $ DiscardedEncounterCard $ EncounterCard
          (CardId . unMinionId $ toId minion)
          (getCardDef minion)
      pure $ g & minionsL %~ delete mid
    TreacheryTarget tid -> do
      for_ (lookup tid gameTreacheries) $ \treachery ->
        push $ DiscardedEncounterCard $ EncounterCard
          (CardId . unTreacheryId $ toId treachery)
          (getCardDef treachery)
      pure $ g & treacheriesL %~ delete tid
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
            (activeCostWindow activeCost)
          pure $ g & activeCostL .~ Nothing
    Nothing -> error "no active cost"
  CreatedEffect def source matcher -> do
    effectId <- getRandom
    let effect = createEffect effectId (toCardCode def) source matcher
    pure $ g & effectsL %~ insert effectId effect
  DisabledEffect effectId -> pure $ g & effectsL %~ delete effectId
  PutCardIntoPlay ident card payment mWindow -> do
    case cdCardType (getCardDef card) of
      AllyType -> do
        let ally = createAlly ident card
        pushAll
          [ IdentityMessage ident (AllyCreated $ toId ally)
          , CheckWindows [W.Window After $ W.PlayedAlly (toId ally)]
          ]
        pure $ g & alliesL %~ insert (toId ally) ally
      SupportType -> do
        let support = createSupport ident card
        pushAll
          [ IdentityMessage ident (SupportCreated $ toId support)
          , CheckWindows [W.Window After $ W.PlayedSupport (toId support)]
          ]
        pure $ g & supportsL %~ insert (toId support) support
      UpgradeType -> do
        let upgrade = createUpgrade ident card
        push $ UpgradeMessage (toId upgrade) PlayedUpgrade
        pure $ g & upgradesL %~ insert (toId upgrade) upgrade
      EventType -> do
        let event = createEvent ident card
        pushAll $ map
          (EventMessage (toId event))
          [PlayedEvent ident payment mWindow, ResolvedEvent]
        pure $ g & eventsL %~ insert (toId event) event
      _ -> error "Unhandled"
  RevealEncounterCard ident card -> do
    case cdCardType (getCardDef card) of
      AttachmentType -> do
        let attachment = createAttachment card
        pushAll [AttachmentMessage (toId attachment) RevealAttachment]
        pure $ g & attachmentsL %~ insert (toId attachment) attachment
      MinionType -> do
        let minion = createMinion ident card
        pushAll
          [ CheckWindows [ W.Window When $ W.MinionEnteredPlay $ toId minion ]
          , IdentityMessage ident (MinionEngaged $ toId minion)
          , MinionMessage (toId minion) RevealMinion
          ]
        pure $ g & minionsL %~ insert (toId minion) minion
      TreacheryType -> do
        let treachery = createTreachery card
        pushAll
          [ CheckWindows
            [ W.Window When
                $ W.RevealTreachery (toId treachery) W.FromEncounterDeck
            ]
          , TreacheryMessage (toId treachery) $ RevealTreachery ident
          , TreacheryMessage (toId treachery) ResolvedTreachery
          ]
        pure $ g & treacheriesL %~ insert (toId treachery) treachery
      SideSchemeType -> do
        let sideScheme = createSideScheme card
        pushAll [SideSchemeMessage (toId sideScheme) RevealSideScheme]
        pure $ g & sideSchemesL %~ insert (toId sideScheme) sideScheme
      _ -> error "Unhandled"
  EndCheckWindows -> pure g
  IdentityEndedTurn ident -> pure $ g & usedAbilitiesL . ix ident %~ filter
    ((/= PerTurn 1) . abilityLimit)
  GameOver status -> do
    clearQueue
    pure $ g & stateL .~ Finished status
  CheckWindows windows -> do
    abilities <- getsGame getAbilities
    usedAbilities <- getUsedAbilities
    attrs <- getActivePlayer
    playableCards <- concatMapM
      (\w -> map (, Just w) <$> getWindowPlayableCards w attrs)
      windows
    ident <- toId <$> getActivePlayer
    validAbilities <- filterM
      (andM . sequence
        [ pure . passesUseLimit ident usedAbilities
        , passesCriteria ident
        , \a -> anyM (`abilityInWindow` a) windows
        ]
      )
      abilities
    let forcedAbiltiies = filter isForcedAbility validAbilities
    case (forcedAbiltiies, validAbilities, playableCards) of
      ([], [], []) -> pure ()
      ([], as, cs) ->
        push
          $ Ask ident
          . ChooseOne
          $ Label "Use no responses/interrupts" []
          : map (UseAbility . (choicesL <>~ [Run [CheckWindows windows]])) as
          <> map (uncurry PlayCard) cs
      (forced, _, _) -> push $ Ask ident . ChooseOne $ map
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

getWindowPlayableCards
  :: MonadGame env m => Window -> PlayerIdentity -> m [PlayerCard]
getWindowPlayableCards window player = filterM
  (isWindowPlayable window player)
  cards
  where cards = unHand $ playerIdentityHand player

isWindowPlayable
  :: MonadGame env m => Window -> PlayerIdentity -> PlayerCard -> m Bool
isWindowPlayable window attrs c = do
  resources <- getAvailableResourcesFor c
  modifiedCost <- getModifiedCost attrs c
  passedCriteria <- checkCriteria (cdCriteria def)
  passedWindow <- maybe
    (pure False)
    (\matcher -> windowMatches matcher window GameSource)
    (cdResponseWindow def)
  pure $ length resources >= modifiedCost && passedCriteria && passedWindow
 where
  ident = toId attrs
  def = getCardDef c
  checkCriteria = \case
    IsSelf -> error "Irrelevant"
    OwnsThis -> error "Irrelevant"
    NoCriteria -> pure True
    Never -> pure False
    InHeroForm -> member ident <$> select HeroIdentity
    Unexhausted -> member ident <$> select UnexhaustedIdentity
    SelfMatches identityMatcher ->
      member ident <$> select (IdentityWithId ident <> identityMatcher)
    Criteria xs -> allM checkCriteria xs
    MinionExists m -> selectAny m

abilityInWindow :: MonadGame env m => Window -> Ability -> m Bool
abilityInWindow window a = maybe
  (pure False)
  (\matcher -> windowMatches matcher window source)
  (abilityWindow a)
  where source = abilitySource a

instance RunMessage Game where
  runMessage msg g =
    runPreGameMessage msg g
      >>= traverseOf scenarioL (runMessage msg)
      >>= traverseOf (playersL . each) (runMessage msg)
      >>= traverseOf (alliesL . each) (runMessage msg)
      >>= traverseOf (supportsL . each) (runMessage msg)
      >>= traverseOf (upgradesL . each) (runMessage msg)
      >>= traverseOf (treacheriesL . each) (runMessage msg)
      >>= traverseOf (villainsL . each) (runMessage msg)
      >>= traverseOf (eventsL . each) (runMessage msg)
      >>= traverseOf (effectsL . each) (runMessage msg)
      >>= traverseOf (attachmentsL . each) (runMessage msg)
      >>= traverseOf (minionsL . each) (runMessage msg)
      >>= traverseOf (sideSchemesL . each) (runMessage msg)
      >>= runGameMessage msg

class HasGame a where
  game :: a -> IORef Game

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

createMinion :: IdentityId -> EncounterCard -> Minion
createMinion ident card =
  lookupMinion (toCardCode card) ident (MinionId $ unCardId $ ecCardId card)

createAttachment :: EncounterCard -> Attachment
createAttachment card =
  lookupAttachment (toCardCode card) (AttachmentId $ unCardId $ ecCardId card)

createEffect :: EffectId -> CardCode -> Source -> EntityMatcher -> Effect
createEffect ident cardCode source matcher =
  lookupEffect cardCode source matcher ident

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
  , gameWindows = []
  , gameUsedAbilities = mempty
  , gameActivePlayer = toId player
  , gameScenario = scenario
  , gameAllies = mempty
  , gameSupports = mempty
  , gameUpgrades = mempty
  , gameTreacheries = mempty
  , gameAttachments = mempty
  , gameSideSchemes = mempty
  , gameEvents = mempty
  , gameEffects = mempty
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
  getAbilities g =
    concatMap getAbilities (elems $ gamePlayers g)
      <> concatMap getAbilities (elems $ gameAllies g)
      <> concatMap getAbilities (elems $ gameSupports g)
      <> concatMap getAbilities (elems $ gameUpgrades g)

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
  result <- filterM (matchFilter m) identities
  pure $ HashSet.fromList $ map toId result
 where
  matchFilter x = case x of
    AnyIdentity -> pure . const True
    HeroIdentity -> pure . isHero
    AlterEgoIdentity -> pure . isAlterEgo
    UnexhaustedIdentity -> pure . not . isExhausted
    IdentityWithId ident' -> pure . (== ident') . toId
    IdentityWithDamage gameValueMatcher ->
      gameValueMatches gameValueMatcher . identityDamage
    IdentityMatchAll xs -> andM . traverse matchFilter xs
    You -> \a -> do
      ap <- getActivePlayer
      pure $ a == ap

gameSelectCharacter
  :: MonadGame env m => CharacterMatcher -> m (HashSet CharacterId)
gameSelectCharacter = \case
  CharacterWithDamage gameValueMatcher -> do
    villains <- map VillainCharacter
      <$> selectList (VillainWithDamage gameValueMatcher)
    minions <- map MinionCharacter
      <$> selectList (MinionWithDamage gameValueMatcher)
    identities <- map IdentityCharacter
      <$> selectList (IdentityWithDamage gameValueMatcher)
    allies <- map AllyCharacter <$> selectList (AllyWithDamage gameValueMatcher)
    pure . HashSet.fromList $ villains <> minions <> identities <> allies

gameSelectAlly :: MonadGame env m => AllyMatcher -> m (HashSet AllyId)
gameSelectAlly m = do
  allies <- toList <$> getsGame gameAllies
  result <- filterM (matchFilter m) allies
  pure $ HashSet.fromList $ map toId result
 where
  matchFilter x = case x of
    UnexhaustedAlly -> pure . not . isExhausted
    ExhaustedAlly -> pure . isExhausted
    AllyControlledBy identityMatcher -> \ally -> do
      identities <- select identityMatcher
      pure $ member (getAllyController ally) identities
    AllyWithUses gameValueMatcher ->
      gameValueMatches gameValueMatcher . getAllyUses
    AllyWithDamage gameValueMatcher ->
      gameValueMatches gameValueMatcher . getAllyDamage

gameSelectSupport :: MonadGame env m => SupportMatcher -> m (HashSet SupportId)
gameSelectSupport = \case
  UnexhaustedSupport -> do
    supports <- toList <$> getsGame gameSupports
    pure $ HashSet.fromList $ map toId $ filter (not . isExhausted) supports
  SupportControlledBy identityMatcher -> do
    supports <- toList <$> getsGame gameSupports
    identities <- select identityMatcher
    pure $ HashSet.fromList $ map toId $ filter
      ((`member` identities) . getSupportController)
      supports

gameSelectUpgrade :: MonadGame env m => UpgradeMatcher -> m (HashSet UpgradeId)
gameSelectUpgrade = \case
  UnexhaustedUpgrade -> do
    upgrades <- toList <$> getsGame gameUpgrades
    pure $ HashSet.fromList $ map toId $ filter (not . isExhausted) upgrades
  UpgradeWithUses gameValueMatcher -> do
    upgrades <- toList <$> getsGame gameUpgrades
    HashSet.fromList
      . map toId
      <$> filterM (gameValueMatches gameValueMatcher . getUpgradeUses) upgrades
  UpgradeControlledBy identityMatcher -> do
    upgrades <- toList <$> getsGame gameUpgrades
    identities <- select identityMatcher
    pure $ HashSet.fromList $ map toId $ filter
      ((`member` identities) . getUpgradeController)
      upgrades

gameSelectEnemy :: MonadGame env m => EnemyMatcher -> m (HashSet EnemyId)
gameSelectEnemy = \case
  EnemyWithId enemyId -> case enemyId of
    EnemyVillainId villainId ->
      HashSet.map EnemyVillainId <$> gameSelectVillain (VillainWithId villainId)
    EnemyMinionId minionId ->
      HashSet.map EnemyMinionId <$> gameSelectMinion (MinionWithId minionId)
  AnyEnemy -> do
    villains <- toList <$> getsGame gameVillains
    minions <- toList <$> getsGame gameMinions
    pure
      $ HashSet.fromList
      $ map (EnemyVillainId . toId) villains
      <> map (EnemyMinionId . toId) minions

gameSelectVillain :: MonadGame env m => VillainMatcher -> m (HashSet VillainId)
gameSelectVillain m = do
  villains <- toList <$> getsGame gameVillains
  result <- filterM (matchFilter m) villains
  pure $ HashSet.fromList $ map toId result
 where
  matchFilter x = case x of
    ActiveVillain -> pure . const True
    VillainWithId ident' -> pure . (== ident') . toId
    VillainWithDamage gameValueMatcher ->
      gameValueMatches gameValueMatcher . villainDamage

gameSelectMinion :: MonadGame env m => MinionMatcher -> m (HashSet MinionId)
gameSelectMinion m = do
  minions <- toList <$> getsGame gameMinions
  result <- filterM (matchFilter m) minions
  pure $ HashSet.fromList $ map toId result
 where
  matchFilter x = case x of
    AnyMinion -> pure . const True
    MinionWithId ident' -> pure . (== ident') . toId
    MinionWithDamage gameValueMatcher ->
      gameValueMatches gameValueMatcher . getMinionDamage

gameSelectTreachery
  :: MonadGame env m => TreacheryMatcher -> m (HashSet TreacheryId)
gameSelectTreachery = \case
  AnyTreachery -> do
    treacheries <- toList <$> getsGame gameTreacheries
    pure $ HashSet.fromList $ map toId treacheries

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

getModifiers :: (MonadGame env m, IsSource a, IsTarget a) => a -> m [Modifier]
getModifiers a = do
  effects <- toList <$> getsGame gameEffects
  concatMapM (getModifiersFor (toSource a) (toTarget a)) effects

getCurrentWindows :: MonadGame env m => m [Window]
getCurrentWindows = do
  windows <- getsGame gameWindows
  pure $ case windows of
    [] -> []
    x : _ -> x
