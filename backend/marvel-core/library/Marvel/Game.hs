{-# LANGUAGE TemplateHaskell #-}
module Marvel.Game where

import Marvel.Prelude

import qualified Data.Aeson.Diff as Diff
import Marvel.Ability
import Marvel.AlterEgo.Cards
import Marvel.Card.Code
import Marvel.Card.Def
import Marvel.Card.PlayerCard
import Marvel.Debug
import Marvel.Deck
import Marvel.Entity
import Marvel.Exception
import Marvel.Hero.Cards
import Marvel.Id
import Marvel.Identity
import Marvel.Message
import Marvel.Phase
import Marvel.Question
import Marvel.Queue
import Marvel.Resource
import Marvel.Scenario
import Marvel.Villain

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
  , gameQuestion :: HashMap IdentityId Question
  , gameUsedAbilities :: HashMap IdentityId [Ability]
  , gameActivePlayer :: IdentityId

  -- leave last for `newGame`
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

runGameMessage :: MonadGame env m => Message -> Game -> m Game
runGameMessage msg g@Game {..} = case msg of
  StartGame -> do
    push StartScenario
    pushAll $ map (`IdentityMessage` SideMessage SetupIdentity) gamePlayerOrder
    case gamePlayerOrder of
      [] -> throwM NoPlayers
      [p] -> push (SetPlayerOrder [p])
      players@(p : _) -> choosePlayerOrder p players
    pure $ g { gameState = InProgress }
  SetPlayerOrder xs -> pure $ g { gamePlayerOrder = xs }
  AddVillain cardCode -> do
    villainId <- getRandom
    case lookupVillain cardCode villainId of
      Just x -> pure $ g & villainsL . at villainId ?~ x
      Nothing -> throwM $ MissingCardCode "AddVillain" cardCode
  UsedAbility ident a -> pure $ g & usedAbilitiesL %~ insertWith (<>) ident [a]
  _ -> pure g

instance RunMessage Game where
  runMessage msg g =
    traverseOf scenarioL (runMessage msg) g
      >>= traverseOf (playersL . each) (runMessage msg)
      >>= runGameMessage msg

class HasGame a where
  game :: a -> IORef Game

newGame :: PlayerIdentity -> Scenario -> Game
newGame player scenario = Game
  { gamePhase = PlayerPhase
  , gameState = Unstarted
  , gamePlayerOrder = [toId player]
  , gamePlayers = fromList [(toId player, player)]
  , gameVillains = mempty
  , gameQuestion = mempty
  , gameUsedAbilities = mempty
  , gameActivePlayer = toId player
  , gameScenario = scenario
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

runGameMessages :: (MonadGame env m, CoerceRole m) => m ()
runGameMessages = do
  mMsg <- pop
  -- debug =<< getGame
  for_ mMsg debug
  for_ mMsg $ \case
    Ask ident choices -> do
      withGame_ $ questionL .~ fromList [(ident, choices)]
    other -> do
      withGameM $ runMessage other
      runGameMessages

-- TODO: implement this for api
replayChoices :: MonadGame env m => [Diff.Patch] -> m ()
replayChoices _ = pure ()

getAvailableResourcesFor :: MonadGame env m => PlayerCard -> m [Resource]
getAvailableResourcesFor c = do
  players <- toList <$> getsGame gamePlayers
  pure $ concatMap (`resourcesFor` c) players