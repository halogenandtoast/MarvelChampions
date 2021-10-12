module Marvel.Queue where

import Marvel.Prelude

import Marvel.Message

type Queue = [Message]

class HasQueue a where
  queueL :: Lens' a (IORef Queue)

withQueue
  :: (HasQueue env, MonadReader env m, MonadIO m)
  => (Queue -> (Queue, a))
  -> m a
withQueue body = do
  queue <- asks $ view queueL
  atomicModifyIORef' queue body

withQueue_
  :: (HasQueue env, MonadReader env m, MonadIO m) => (Queue -> Queue) -> m ()
withQueue_ body = withQueue $ (, ()) . body

push :: (HasQueue env, MonadReader env m, MonadIO m) => Message -> m ()
push = withQueue_ . (:)

pushAll :: (HasQueue env, MonadReader env m, MonadIO m) => [Message] -> m ()
pushAll = withQueue_ . (<>)

pop :: (HasQueue env, MonadReader env m, MonadIO m) => m (Maybe Message)
pop = withQueue \case
  [] -> ([], Nothing)
  (x : xs) -> (xs, Just x)
