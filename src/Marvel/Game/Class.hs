module Marvel.Game.Class where

import Control.Monad.IO.Class (MonadIO)
import Control.Monad.Reader (ReaderT, ask, lift)
import Marvel.Debug (HasDebugLogger)
import {-# SOURCE #-} Marvel.Game.Base (Game)
import Marvel.Prelude
import Marvel.Queue (HasQueue (..))

class (Monad m) => HasGame m where
  getGame :: m Game

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
