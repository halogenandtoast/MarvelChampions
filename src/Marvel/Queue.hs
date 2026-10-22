module Marvel.Queue where

import Marvel.Prelude

import Control.Monad.IO.Class as X (MonadIO (..))
import Data.IORef
import {-# SOURCE #-} Marvel.Message

type Queue = [Message]

class (MonadIO m) => HasQueue m where
  getQueue :: m (IORef Queue)

  -- | Put the messages, in the order given, ahead of what is queued. A 'QueueT' holds
  -- them back until its handler is done.
  pushAll :: [Message] -> m ()
  pushAll = withQueue_ . (<>)

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
push = pushAll . pure

pushWhen :: (HasQueue m) => Bool -> Message -> m ()
pushWhen b = when b . push

pushWhenM :: (HasQueue m) => m Bool -> Message -> m ()
pushWhenM b = whenM b . push

pop :: (HasQueue m) => m (Maybe Message)
pop = withQueue \case
  [] -> ([], Nothing)
  (x : xs) -> (xs, Just x)

-- | Whether a message matching is queued, held in a 'Simultaneously' or not.
isQueued :: (HasQueue m) => (Message -> Bool) -> m Bool
isQueued f = withQueue $ \q -> (q, any (matchesWithin f) q)

matchesWithin :: (Message -> Bool) -> Message -> Bool
matchesWithin f msg = f msg || maybe False (any (matchesWithin f)) (simultaneousMessages msg)

cancelMatchingMessage :: (HasQueue m) => (Message -> Bool) -> m ()
cancelMatchingMessage = replaceMatchingMessage (const [])

replaceMatchingMessage ::
  (HasQueue m) => (Message -> [Message]) -> (Message -> Bool) -> m ()
replaceMatchingMessage spliceF f = withQueue_ go
 where
  go q = case break (matchesWithin f) q of
    (pre, []) -> pre
    (pre, msg : rest)
      | f msg -> pre <> spliceF msg <> rest
      | otherwise -> pre <> maybe [msg] (pure . simultaneously . go) (simultaneousMessages msg) <> rest
