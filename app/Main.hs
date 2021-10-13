module Main where

import Marvel.Prelude

import Control.Monad.Catch
import Data.HashMap.Strict qualified as HashMap
import Data.Text qualified as T
import Marvel.Game
import Marvel.Identity.Attrs
import Marvel.Message
import Marvel.Queue
import Marvel.Scenario
import System.IO (hFlush)

data Env = Env
  { envGame :: IORef Game
  , envQueue :: IORef Queue
  }

newtype AppT a = AppT { unAppT :: ReaderT Env IO a }
  deriving newtype
    ( Functor
    , Applicative
    , Monad
    , MonadIO
    , MonadReader Env
    , MonadRandom
    , MonadThrow
    , MonadCatch
    )

instance MonadGame Env AppT

instance HasGame Env where
  gameL = lens envGame \m x -> m { envGame = x }

instance HasQueue Env where
  queueL = lens envQueue \m x -> m { envQueue = x }

newtype GameError = GameError { unGameError :: String }
  deriving newtype Show

instance Exception GameError

runApp :: (MonadCatch m, MonadIO m) => Env -> AppT a -> m a
runApp env body = handleAll handler $
  liftIO $ runReaderT (unAppT body) env
  where handler e = throwM $ GameError $ displayException e

createAndRunGame :: AppT ()
createAndRunGame = do
  createPlayer "01001"
  createPlayer "01010"
  push StartGame
  runGame

runGame :: AppT ()
runGame = do
  runGameMessages
  game <- getGame
  case HashMap.toList (gameQuestion game) of
    [(ident, question)] -> do
      messages <- handleQuestion ident question
      pushAll messages
      withGame_ $ questionL .~ mempty
      runGame
    _ -> pure ()

keepAsking :: (Show a, Read a, MonadIO m) => Text -> m a
keepAsking s = do
  putStr $ T.unpack s
  liftIO $ hFlush stdout
  mresult <- readMaybe . T.unpack <$> getLine
  case mresult of
    Nothing -> keepAsking s
    Just a -> pure a

handleQuestion :: MonadIO m => IdentityId -> Question -> m [Message]
handleQuestion _ = \case
  ChooseOne [] -> pure []
  ChooseOne choices -> do
    i <- keepAsking
      ("Choose one:\n\n"
      <> unlines (zipWith (curry tshow) [1 :: Int ..] choices)
      )
    pure . concatMap choiceMessages . maybeToList $ choices !!? (i - 1)

newEnv :: Scenario -> IO Env
newEnv scenario = Env <$> newIORef (newGame scenario) <*> newIORef []

main :: IO ()
main = case lookupScenario "01094" of
  Just scenario -> do
    env <- newEnv scenario
    runApp env createAndRunGame
    print =<< readIORef (envGame env)
    print =<< readIORef (envQueue env)
  Nothing -> error "Invalid Scenario"
