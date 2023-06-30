module Marvel.Queue where

import Marvel.Prelude

import Control.Monad.IO.Class as X (MonadIO (..))
import Data.IORef
import {-# SOURCE #-} Marvel.Message

type Queue = [Message]

class (MonadIO m) => HasQueue m where
  getQueue :: m (IORef Queue)

withQueue :: (HasQueue m) => (Queue -> (Queue, a)) -> m a
withQueue body = do
  ref <- getQueue
  liftIO $ atomicModifyIORef' ref body

withQueue_ :: (HasQueue m) => (Queue -> Queue) -> m ()
withQueue_ body = withQueue $ (,()) . body

withQueueM :: (HasQueue m) => (Queue -> m a) -> m a
withQueueM body = withQueue (toSnd id) >>= body

clearQueue :: (HasQueue m) => m ()
clearQueue = withQueue_ (const mempty)

push :: (HasQueue m) => Message -> m ()
push = withQueue_ . (:)

pushWhen :: (HasQueue m) => Bool -> Message -> m ()
pushWhen b = when b . push

pushWhenM :: (HasQueue m) => m Bool -> Message -> m ()
pushWhenM b = whenM b . push

pushAll :: (HasQueue m) => [Message] -> m ()
pushAll = withQueue_ . (<>)

pop :: (HasQueue m) => m (Maybe Message)
pop = withQueue \case
  [] -> ([], Nothing)
  (x : xs) -> (xs, Just x)

cancelMatchingMessage :: (HasQueue m) => (Message -> Bool) -> m ()
cancelMatchingMessage = replaceMatchingMessage (const [])

replaceMatchingMessage ::
  (HasQueue m) => (Message -> [Message]) -> (Message -> Bool) -> m ()
replaceMatchingMessage spliceF f = withQueue_ $ \q -> case break f q of
  (pre, []) -> pre
  (pre, msg : rest) -> pre <> spliceF msg <> rest
