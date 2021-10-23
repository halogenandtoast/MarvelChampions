module Marvel.Game where

import Marvel.Prelude

import Marvel.AlterEgo.Cards
import Marvel.Card.Code
import Marvel.Debug
import Marvel.Entity
import Marvel.Exception
import Marvel.Hero.Cards
import Marvel.Identity
import Marvel.Message
import Marvel.Phase
import Marvel.Question
import Marvel.Queue
import Marvel.Scenario
import Marvel.Villain
import Marvel.Villain.Attrs

data GameState = Unstarted | InProgress | Finished
  deriving stock Show

data Game = Game
  { gamePhase :: Phase
  , gameState :: GameState
  , -- players in player order
    gamePlayerOrder :: [IdentityId]
  , gamePlayers :: HashMap IdentityId PlayerIdentity
  , gameQuestion :: HashMap IdentityId Question
  , gameVillains :: HashMap VillainId Villain
  , gameScenario :: Scenario
  }
  deriving stock Show

getPlayers :: MonadGame env m => m [IdentityId]
getPlayers = getsGame gamePlayerOrder

runGameMessage :: MonadGame env m => Message -> Game -> m Game
runGameMessage msg g@Game {..} = case msg of
  StartGame -> do
    push StartScenario
    case gamePlayerOrder of
      [] -> throwM NoPlayers
      players@(p : _) -> choosePlayerOrder p players
    pure $ g { gameState = InProgress }
  SetPlayerOrder xs -> pure $ g { gamePlayerOrder = xs }
  AddVillain cardCode -> do
    villainId <- getRandom
    case lookupVillain cardCode villainId of
      Just x -> pure $ g & villainsL . at villainId ?~ x
      Nothing -> throwM $ MissingCardCode "AddVillain" cardCode
  _ -> pure g

instance RunMessage Game where
  runMessage msg g =
    traverseOf scenarioL (runMessage msg) g
      >>= traverseOf (playersL . each) (runMessage msg)
      >>= runGameMessage msg

playerOrderL :: Lens' Game [IdentityId]
playerOrderL = lens gamePlayerOrder \m x -> m { gamePlayerOrder = x }

playersL :: Lens' Game (HashMap IdentityId PlayerIdentity)
playersL = lens gamePlayers \m x -> m { gamePlayers = x }

questionL :: Lens' Game (HashMap IdentityId Question)
questionL = lens gameQuestion \m x -> m { gameQuestion = x }

scenarioL :: Lens' Game Scenario
scenarioL = lens gameScenario \m x -> m { gameScenario = x }

villainsL :: Lens' Game (HashMap VillainId Villain)
villainsL = lens gameVillains \m x -> m { gameVillains = x }

class HasGame a where
  game :: a -> IORef Game

newGame :: Scenario -> Game
newGame = Game PlayerPhase Unstarted mempty mempty mempty mempty

addPlayer :: MonadGame env m => PlayerIdentity -> m ()
addPlayer player = withGame_
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
  => MonadGame env m | env -> m

createPlayer :: MonadGame env m => CardCode -> m ()
createPlayer cardCode = do
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
      addPlayer $ createIdentity alterEgoSide heroSide
    _ -> error "stuff"

getGame :: MonadGame env m => m Game
getGame = readIORef =<< asks game

runGameMessages :: (MonadGame env m, CoerceRole m) => m ()
runGameMessages = do
  mMsg <- pop
  debug =<< getGame
  for_ mMsg debug
  for_ mMsg $ \case
    Ask ident choices -> do
      withGame_ $ questionL .~ fromList [(ident, choices)]
    other -> do
      withGameM $ runMessage other
      runGameMessages
