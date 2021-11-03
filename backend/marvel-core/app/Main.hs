{-# LANGUAGE QuasiQuotes #-}
{-# OPTIONS_GHC -Wno-deprecations #-}
module Main where

import Marvel.Prelude

import qualified Data.Array as A
import qualified Data.Map.Strict as Map
import qualified Data.Text as T
import Marvel.Card.Code
import Marvel.Debug
import Marvel.Deck
import Marvel.Game
import Marvel.Id
import Marvel.Identity
import Marvel.Message
import Marvel.PlayerCard
import Marvel.Question
import Marvel.Queue
import Marvel.Scenario
import Network.HTTP.Simple
import System.IO (hFlush)
import Text.Pretty.Simple
import Text.Regex.Posix
import URI.ByteString (URI, pathL, serializeURIRef')
import URI.ByteString.QQ

data Env = Env
  { envGame :: IORef Game
  , envQueue :: IORef Queue
  , envDebugLogger :: Maybe DebugLogger
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
  game = envGame

instance HasQueue Env where
  queue = envQueue

instance HasDebugLogger Env where
  debugLogger = envDebugLogger

newtype GameError = GameError { unGameError :: String }
  deriving newtype Show

instance Exception GameError

runApp :: MonadIO m => Env -> AppT a -> m a
runApp env body = liftIO $ handleAll handler $ runReaderT (unAppT body) env
  where handler e = throwM $ GameError $ displayException e

-- | Create and run a game
-- This is specific to testing in the terminal UI
createAndRunGame :: AppT ()
createAndRunGame = do
  runGame

createPlayerFromDecklistUrl :: URI -> IO PlayerIdentity
createPlayerFromDecklistUrl url = do
  res <- httpJSON
    =<< parseRequest (decodeUtf8 . serializeURIRef' $ toApiUrl url)
  loadDecklist $ getResponseBody res

data Decklist = Decklist
  { investigator_code :: CardCode
  , slots :: Map CardCode Int
  }
  deriving stock (Show, Generic)
  deriving anyclass FromJSON

loadDecklist :: Decklist -> IO PlayerIdentity
loadDecklist decklist = do
  let ident = toBaseCardCode $ investigator_code decklist
  initPlayer ident =<< toDeck decklist

toDeck :: Decklist -> IO Deck
toDeck =
  fmap Deck
    . traverse toCard
    . concatMap (uncurry (flip replicate))
    . Map.toList
    . slots

toCard :: CardCode -> IO PlayerCard
toCard code = PlayerCard <$> getRandom <*> pure (lookupPlayerCard code)

toApiUrl :: URI -> URI
toApiUrl url = url & pathL %~ toPublicDeckPath

toPublicDeckPath :: ByteString -> ByteString
toPublicDeckPath bs = "/api/public/decklist/" <> matches <> ".json"
 where
  matches :: ByteString
  matches =
    case
        matchOnceText
          (makeRegex @_ @_ @_ @ByteString "decklist/view/([0-9]+)" :: Regex)
          bs
      of
        Nothing -> error "failed"
        Just (_, mt, _) -> fst $ mt A.! 1

runGame :: AppT ()
runGame = do
  runGameMessages
  question <- getsGame gameQuestion
  case toPairs question of
    [(ident, q)] -> do
      messages <- handleQuestion ident q
      pushAll messages
      withGame_ $ questionL .~ mempty
      runGame
    _ -> pure ()

keepAsking :: (Read a, MonadIO m) => Text -> m a
keepAsking s = do
  putStr $ T.unpack s <> "> "
  liftIO $ hFlush stdout
  mresult <- readMaybe . T.unpack <$> getLine
  maybe (keepAsking s) pure mresult

asOptions :: Show a => [a] -> [Text]
asOptions = zipWith (curry tshow) [1 :: Int ..]

handleQuestion :: MonadIO m => IdentityId -> Question -> m [Message]
handleQuestion ident = \case
  ChooseOne [] -> pure []
  ChooseOne choices -> do
    i <- keepAsking ("Choose one:\n\n" <> unlines (asOptions choices))
    pure . concatMap (choiceMessages ident) . maybeToList $ choices !!? (i - 1)
  ChoosePlayerOrder (Unsorted []) (Sorted ys) -> pure [SetPlayerOrder ys]
  ChoosePlayerOrder (Unsorted [x]) (Sorted ys) ->
    pure [SetPlayerOrder $ ys ++ [x]]
  ChoosePlayerOrder unsorted@(Unsorted xs) sorted@(Sorted ys) -> do
    let idx = length ys + 1
    i <- keepAsking
      ("Choose player " <> show idx <> ":\n\n" <> unlines (asOptions xs))
    case xs !!? (i - 1) of
      Just n -> pure
        [ Ask ident $ ChoosePlayerOrder
            (Unsorted $ filter (/= n) xs)
            (Sorted $ ys ++ [n])
        ]
      Nothing -> pure [Ask ident $ ChoosePlayerOrder unsorted sorted]

prettyLogger :: DebugLogger
prettyLogger = DebugLogger pPrint

newEnv :: PlayerIdentity -> Scenario -> IO Env
newEnv player scenario =
  Env <$> newIORef (newGame player scenario) <*> newIORef [StartGame] <*> pure
    (Just prettyLogger)

main :: IO ()
main = case lookupScenario "01094" of
  Just scenario -> do
    player <-
      createPlayerFromDecklistUrl
        [uri|https://marvelcdb.com/decklist/view/103/spider-man-and-friends-solo-play-expert-mode-1.0|]
    env <- newEnv player scenario
    runApp env createAndRunGame
    pPrint =<< readIORef (envGame env)
    pPrint =<< readIORef (envQueue env)
  Nothing -> error "Invalid Scenario"
