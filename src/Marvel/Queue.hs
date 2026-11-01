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
matchesWithin f message = f message || f (queuedMessage message) || any (matchesWithin f . fst) (queuedChildEdits message)

cancelMatchingMessage :: HasQueue m => (Message -> Bool) -> m ()
cancelMatchingMessage = replaceMatchingMessage (const [])

replaceMatchingMessage :: HasQueue m => (Message -> [Message]) -> (Message -> Bool) -> m ()
replaceMatchingMessage splice predicate = withQueue_ $ \queue -> case break (matchesWithin predicate) queue of
  (before, []) -> before
  (before, message : after) -> before <> within message <> after
 where
  within message
    | predicate message = splice message
    | predicate (queuedMessage message) = rewrapQueuedMessages message (splice $ queuedMessage message)
    | otherwise = case [(child,rebuild) | (child,rebuild) <- queuedChildEdits message, matchesWithin predicate child] of
        (child,rebuild) : _ -> rebuild $ within child
        [] -> [message]
