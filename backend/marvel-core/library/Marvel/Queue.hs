module Marvel.Queue where

import Marvel.Prelude

import Marvel.Message

type Queue = [Message]

class HasQueue a where
   queue :: a -> IORef Queue

withQueue
  :: (HasQueue env, MonadReader env m, MonadIO m)
  => (Queue -> (Queue, a))
  -> m a
withQueue body = do
  ref <- asks queue
  atomicModifyIORef' ref body

withQueue_
  :: (HasQueue env, MonadReader env m, MonadIO m) => (Queue -> Queue) -> m ()
withQueue_ body = withQueue $ (, ()) . body

clearQueue :: (HasQueue env, MonadReader env m, MonadIO m) => m ()
clearQueue = withQueue_ (const mempty)

push :: (HasQueue env, MonadReader env m, MonadIO m) => Message -> m ()
push = withQueue_ . (:)

pushAll :: (HasQueue env, MonadReader env m, MonadIO m) => [Message] -> m ()
pushAll = withQueue_ . (<>)

pop :: (HasQueue env, MonadReader env m, MonadIO m) => m (Maybe Message)
pop = withQueue \case
  [] -> ([], Nothing)
  (x : xs) -> (xs, Just x)

cancelMatchingMessage
  :: (HasQueue env, MonadReader env m, MonadIO m) => (Message -> Bool) -> m ()
cancelMatchingMessage f = withQueue_ $ \q -> case break f q of
  (pre, []) -> pre
  (pre, _ : rest) -> pre <> rest

replaceMatchingMessage
  :: (HasQueue env, MonadReader env m, MonadIO m) => [Message] -> (Message -> Bool) -> m ()
replaceMatchingMessage splice f = withQueue_ $ \q -> case break f q of
  (pre, []) -> pre
  (pre, _ : rest) -> pre <> splice <> rest
