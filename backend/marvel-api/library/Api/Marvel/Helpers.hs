{-# LANGUAGE TupleSections #-}
module Api.Marvel.Helpers where

import Import hiding (appLogger)

import Control.Concurrent.STM.TChan
import Control.Monad.Catch (MonadCatch, MonadThrow)
import Control.Monad.Random (MonadRandom(..))
import qualified Data.ByteString.Lazy as BSL
import qualified Data.Map.Strict as Map
import Marvel.Card.Code
import Marvel.Debug
import Marvel.Game
import Marvel.Message
import Marvel.PlayerCard
import Marvel.Queue

data ApiResponse = GameUpdate Game | GameMessage Text
  deriving stock Generic
  deriving anyclass ToJSON

newtype GameAppT a = GameAppT { unGameAppT :: ReaderT GameApp IO a }
  deriving newtype (MonadReader GameApp, Functor, Applicative, Monad, MonadFail, MonadIO, MonadRandom, MonadCatch, MonadThrow)

data GameApp = GameApp
  { appGame :: IORef Game
  , appQueue :: IORef [Message]
  , appDebugLogger :: Maybe DebugLogger
  }

newApp :: MonadIO m => Game -> [Message] -> m GameApp
newApp g msgs = do
  gameRef <- newIORef g
  queueRef <- newIORef msgs
  pure $ GameApp gameRef queueRef Nothing

instance HasGame GameApp where
  game = appGame

instance HasQueue GameApp where
  queue = appQueue

instance HasDebugLogger GameApp where
  debugLogger = appDebugLogger

instance MonadGame GameApp GameAppT

runGameApp :: MonadIO m => GameApp -> GameAppT a -> m a
runGameApp gameApp = liftIO . flip runReaderT gameApp . unGameAppT

noLogger :: Applicative m => Text -> m ()
noLogger = const (pure ())

getChannel :: MarvelGameId -> Handler (TChan BSL.ByteString)
getChannel gameId = do
  gameChannelsRef <- appGameChannels <$> getYesod
  gameChannels <- readIORef gameChannelsRef
  case Map.lookup gameId gameChannels of
    Just chan -> pure chan
    Nothing -> do
      chan <- atomically newBroadcastTChan
      atomicModifyIORef' gameChannelsRef
        $ \gameChannels' -> (Map.insert gameId chan gameChannels', ())
      pure chan

toDeck :: MarvelDBDecklist -> IO [PlayerCard]
toDeck =
  traverse toCard . concatMap (uncurry (flip replicate)) . Map.toList . slots

toCard :: CardCode -> IO PlayerCard
toCard code = PlayerCard <$> getRandom <*> pure (lookupPlayerCard code)

loadDecklist :: MarvelDeck -> IO (CardCode, [PlayerCard])
loadDecklist marvelDeck = (heroCardCode, ) <$> toDeck decklist
 where
  decklist = marvelDeckList marvelDeck
  heroCardCode = investigator_code decklist
