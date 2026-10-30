-- | Detect calculations that stall before the engine can ask its next question.
module SmokeMonitor (SmokeQuestionTimeout, withQuestionMonitor) where

import Prelude
import Control.Concurrent (forkIO, killThread, myThreadId, threadDelay, throwTo)
import Control.Exception (Exception (..), asyncExceptionFromException, asyncExceptionToException, bracket)
import Data.IORef
import Data.Word (Word64)
import GHC.Clock (getMonotonicTimeNSec)

data SmokeQuestionTimeout = SmokeQuestionTimeout Int (Maybe Int) Int
  deriving stock Show

-- Asynchronous cancellation must not be swallowed by a debug-rendering catch.
instance Exception SmokeQuestionTimeout where
  toException = asyncExceptionToException
  fromException = asyncExceptionFromException
  displayException (SmokeQuestionTimeout micros version count) =
    "Smoke watchdog: no new question within " <> show (fromIntegral micros / 1000000 :: Double)
      <> " seconds; last question version=" <> show version
      <> "; questions observed=" <> show count

data Heartbeat
  = Running Word64 (Maybe Int) Int
  | Expired (Maybe Int) Int

-- | Only a new question version resets the monotonic deadline. The atomic expiry
-- latch makes a heartbeat arriving at the deadline race with the timeout safely.
-- Each game owns its timer, which is stopped on either completion or an exception.
withQuestionMonitor :: Int -> ((Int -> IO ()) -> IO a) -> IO a
withQuestionMonitor timeoutMicros action = do
  owner <- myThreadId
  started <- getMonotonicTimeNSec
  heartbeat <- newIORef (Running started Nothing 0)
  let deadline = fromIntegral timeoutMicros * 1000 :: Word64
      question version = do
        now <- getMonotonicTimeNSec
        atomicModifyIORef' heartbeat $ \state -> case state of
          Running _ previous count | previous /= Just version -> (Running now (Just version) (count + 1), ())
          _ -> (state, ())
      watch = do
        now <- getMonotonicTimeNSec
        expired <- atomicModifyIORef' heartbeat $ \state -> case state of
          Running lastAsked version count
            | now >= lastAsked && now - lastAsked >= deadline -> (Expired version count, Just (version, count))
          Expired version count -> (state, Just (version, count))
          _ -> (state, Nothing)
        case expired of
          Just (version, count) -> throwTo owner $ SmokeQuestionTimeout timeoutMicros version count
          Nothing -> threadDelay (min 50000 timeoutMicros) >> watch
  bracket (forkIO watch) killThread (const $ action question)
