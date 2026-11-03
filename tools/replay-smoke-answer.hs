{-# LANGUAGE OverloadedStrings #-}
-- Replay a SMOKE_FAILURE_SNAPSHOT .previous.json file with the saved RNG.
-- After building the test library, run:
-- stack exec -- runghc -package=MarvelChampions tools/replay-smoke-answer.hs SNAPSHOT
-- Output: original/next question versions, message count, elapsed seconds,
-- public-view bytes, then the fifteen slowest message intervals.

import Marvel.Prelude
import Marvel.Game
import Marvel.Game.Run (answerQuestion)
import Marvel.Game.Public
import Marvel.Message (Message)
import Marvel.Id (IdentityId)
import Marvel.Debug (DebugLogger (..))
import Marvel.Scenario ()
import Data.Word (Word64)
import Data.Text qualified as T
import Data.ByteString.Lazy qualified as BL
import System.Random.Internal (StdGen (..))
import System.Random.SplitMix (seedSMGen')
import System.Environment (getArgs)
import GHC.Clock (getMonotonicTimeNSec)
import Control.Exception (evaluate)
import Data.IORef
import Data.List (sortOn)
import Data.Ord (Down (..))

data Replay = Replay Game [Message] (Word64,Word64) IdentityId Int
instance FromJSON Replay where
  parseJSON = withObject "Replay" $ \o -> Replay <$> o .: "game" <*> o .: "queue" <*> o .: "generator" <*> o .: "player" <*> o .: "choice"
main = do
  args <- getArgs
  path <- case args of
    [file] -> pure file
    _ -> fail "Usage: replay-smoke-answer.hs SNAPSHOT.previous.json"
  snapshot <- decodeFileStrict path
  Replay game queue generator player choice <- maybe (fail "Invalid smoke answer snapshot") pure snapshot
  count <- newIORef (0 :: Int)
  previous <- newIORef Nothing
  timings <- newIORef []
  app <- newGameApp game queue (StdGen $ seedSMGen' generator) (const $ pure ())
  let debug m = do
        _ <- evaluate $ T.length $ tshow m
        modifyIORef' count (+1)
        now <- getMonotonicTimeNSec
        prior <- readIORef previous
        for_ prior $ \(thenTime,label) -> modifyIORef' timings ((now-thenTime,label):)
        writeIORef previous $ Just (now,T.take 160 $ tshow m)
      logging = app {appDebugLogger = Just $ DebugLogger debug}
  before <- getMonotonicTimeNSec
  runGameApp logging $ answerQuestion player choice
  after <- getMonotonicTimeNSec
  priorEnd <- readIORef previous
  for_ priorEnd $ \(thenTime,label) -> modifyIORef' timings ((after-thenTime,label):)
  result <- runGameApp logging getGame
  bytes <- BL.length . encode <$> publicGameFor (const True) result
  messages <- readIORef count
  print (gameQuestionVersion game,gameQuestionVersion result,messages,fromIntegral (after-before)/1e9 :: Double,bytes)

  detailed <- readIORef timings
  for_ (take 15 $ sortOn (Down . fst) detailed) $ \(time,label) -> print (fromIntegral time/1e9 :: Double,label)
