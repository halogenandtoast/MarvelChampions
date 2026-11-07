module Marvel.Game.App where

import Control.Monad.Reader (MonadIO (..), ReaderT (..), asks)
import Data.IORef (atomicModifyIORef', newIORef, readIORef)
import Data.Tuple (swap)
import Marvel.Debug (DebugLogger, HasDebugLogger (..))
import Marvel.Frontend (Frontend (..), FrontendMessage)
import Marvel.Game.Base (Game (..))
import Marvel.Game.Class (HasGame (..), HasGameRef (..), MonadGame)
import Marvel.Message (Message (..), EventMessage (..))
import Marvel.Prelude
import Marvel.Queue (HasQueue (..))
import System.Random (
  StdGen,
  mkStdGen,
  random,
  randomR,
  randomRs,
  randoms,
  splitGen,
 )

newtype GameAppT a = GameAppT {unGameAppT :: ReaderT GameApp IO a}
  deriving newtype (MonadReader GameApp, Functor, Applicative, Monad, MonadFail, MonadIO, MonadCatch, MonadThrow)

{- | A game being played: the game and its queue, the random generator it rolls with
(stored with the game, so a reloaded game rolls the same), and where what it says to
its clients goes.
-}
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

{- | A game to play from its queue, rolling with the generator given and telling its
clients through the function given.
-}
newGameApp :: (MonadIO m) => Game -> [Message] -> StdGen -> (FrontendMessage -> IO ()) -> m GameApp
newGameApp g msgs gen frontend = liftIO do
  gameRef <- newIORef $ restoreResolvingEvents g msgs
  queueRef <- newIORef msgs
  genRef <- newIORef gen
  pure $ GameApp gameRef queueRef Nothing genRef frontend

-- | The game, its queue and its random generator as they now stand.
currentState :: GameApp -> IO (Game, [Message], StdGen)
currentState app = (,,) <$> readIORef (appGame app) <*> readIORef (appQueue app) <*> readIORef (appRandomGen app)

runGameApp :: (MonadIO m) => GameApp -> GameAppT a -> m a
runGameApp gameApp = liftIO . flip runReaderT gameApp . unGameAppT

-- | Old saves retained event entities after resolution. Their queued completion
-- messages identify the events still resolving, regardless of physical card zone.
restoreResolvingEvents :: Game -> [Message] -> Game
restoreResolvingEvents g messages = case gameResolvingEvents g of
  Just _ -> g
  Nothing -> g {gameResolvingEvents = Just $ concatMap pending messages}
 where
  pending (EventMessage eid ResolvedEvent) = [eid]
  pending (Simultaneously messages') = concatMap pending messages'
  pending _ = []
