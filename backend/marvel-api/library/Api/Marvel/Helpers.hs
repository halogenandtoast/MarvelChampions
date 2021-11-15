{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TupleSections #-}
module Api.Marvel.Helpers where

import Import hiding (appLogger)

import Control.Concurrent.STM.TChan
import Control.Monad.Catch (MonadCatch, MonadThrow)
import Control.Monad.Random (MonadRandom(..))
import qualified Data.ByteString.Lazy as BSL
import qualified Data.Map.Strict as Map
import Marvel.Ally
import Marvel.Card.Code
import Marvel.Debug
import Marvel.Deck
import Marvel.Entity (EntityId)
import Marvel.Game
import Marvel.Id
import Marvel.Identity
import Marvel.Message
import Marvel.PlayerCard
import Marvel.Question
import Marvel.Queue
import Marvel.Scenario
import Marvel.Support
import Marvel.Villain

data ApiGame = ApiGame
  { id :: Key MarvelGame
  , name :: Text
  , players :: HashMap (EntityId PlayerIdentity) PlayerIdentity
  , villains :: HashMap (EntityId Villain) Villain
  , scenario :: Scenario
  , question :: HashMap IdentityId Question
  , allies :: HashMap AllyId Ally
  , supports :: HashMap SupportId Support
  }
  deriving stock (Show, Generic)
  deriving anyclass ToJSON

toApiGame :: Entity MarvelGame -> ApiGame
toApiGame (Entity gameId MarvelGame { marvelGameCurrentData, marvelGameName })
  = let Game {..} = marvelGameCurrentData
    in
      ApiGame
        { id = gameId
        , name = marvelGameName
        , players = gamePlayers
        , villains = gameVillains
        , scenario = gameScenario
        , question = gameQuestion
        , allies = gameAllies
        , supports = gameSupports
        }

data ApiResponse = GameUpdate ApiGame | GameMessage Text
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

toDeck :: MarvelDBDecklist -> IO Deck
toDeck =
  fmap Deck
    . traverse toCard
    . concatMap (uncurry (flip replicate))
    . Map.toList
    . slots

toCard :: CardCode -> IO PlayerCard
toCard code = do
  cardId <- getRandom
  pure $ PlayerCard cardId (lookupPlayerCard code) Nothing Nothing

loadDecklist :: MarvelDeck -> IO (CardCode, Deck)
loadDecklist marvelDeck = (heroCardCode, ) <$> toDeck decklist
 where
  decklist = marvelDeckList marvelDeck
  heroCardCode = investigator_code decklist
