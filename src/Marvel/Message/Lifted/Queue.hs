-- | A handler's own queue, as ArkhamHorror's: what a handler run with 'runQueueT'
-- pushes is held back and put on the queue when it is done, in the order it was pushed.
-- Two pushes run as written, not the second first.
module Marvel.Message.Lifted.Queue (
  QueueT,
  runQueueT,
  captureQueueT,
  ReverseQueue (..),
) where

import Marvel.Prelude

import Control.Monad.IO.Class (MonadIO (..))
import Control.Monad.Reader (ReaderT (..), ask, local)
import Control.Monad.Trans (MonadTrans (..))
import Data.IORef (modifyIORef', newIORef, readIORef)
import Marvel.Game.Source
import Marvel.Message
import Marvel.Queue

newtype QueueT m a = QueueT (ReaderT (IORef [Message]) m a)
  deriving newtype (Functor, Applicative, Monad, MonadIO, MonadRandom, MonadThrow)

instance MonadTrans QueueT where
  lift = QueueT . lift

instance (HasGame m) => HasGame (QueueT m) where
  getGame = lift getGame

-- | What looks at the queue, as a cancel does, sees the game's: only pushes are held.
instance (HasQueue m) => HasQueue (QueueT m) where
  getQueue = lift getQueue
  pushAll msgs = QueueT do
    held <- ask
    liftIO $ modifyIORef' held (<> msgs)

-- | Run a handler, then put what it pushed on the queue, in the order pushed.
runQueueT :: (HasQueue m) => QueueT m a -> m a
runQueueT (QueueT body) = do
  held <- liftIO $ newIORef []
  a <- runReaderT body held
  msgs <- liftIO $ readIORef held
  unless (null msgs) $ pushAll msgs
  pure a

-- | Where the lifted helpers ('Marvel.Message.Lifted') run: a handler under 'runQueueT'.
class (HasGame m, HasQueue m, MonadThrow m, MonadRandom m) => ReverseQueue m where
  -- | The messages the body would push, not pushed: what a choice does, say.
  capture :: m () -> m [Message]

instance (HasGame m, HasQueue m, MonadThrow m, MonadRandom m) => ReverseQueue (QueueT m) where
  capture (QueueT body) = QueueT do
    held <- liftIO $ newIORef []
    local (const held) body
    liftIO $ readIORef held

-- | Hold a handler's follow-up messages while preserving its returned state.
-- Simultaneous damage applies each instance before the next taken-damage window,
-- but its after windows and defeat continuations wait until the entire group lands.
captureQueueT :: HasQueue m => QueueT m a -> m (a, [Message])
captureQueueT (QueueT body) = do
  held <- liftIO $ newIORef []
  result <- runReaderT body held
  messages <- liftIO $ readIORef held
  pure (result, messages)
