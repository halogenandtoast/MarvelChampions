{-# LANGUAGE TemplateHaskell #-}

module Api.Handler.Marvel.Games
  ( getApiV1MarvelGameR
  , getApiV1MarvelGameSpectateR
  , getApiV1MarvelGamesR
  , postApiV1MarvelGamesR
  , putApiV1MarvelGameR
  , deleteApiV1MarvelGameR
  , putApiV1MarvelGameRawR
  ) where

import Import hiding (delete, on, (==.))

import Api.Marvel.Helpers
import Api.Marvel.Types.MultiplayerVariant
import Conduit
import Control.Concurrent.STM.TChan
import Control.Lens (view)
import Data.HashMap.Strict qualified as HashMap
import Data.Map.Strict qualified as Map
import Data.Text qualified as T
import Data.Traversable (for)
import Database.Esqueleto.Experimental hiding (update)
import Database.Esqueleto.Internal.Internal (SqlSelect)
import Json
import Marvel.Card.Code
import Marvel.Debug
import Marvel.Entity (toId)
import Marvel.Game
import Marvel.Id
import Marvel.Message
import Marvel.Question
import Marvel.Scenario
import Network.WebSockets (ConnectionException)
import Text.Pretty.Simple
import UnliftIO.Exception (bracket, catch)
import Yesod.WebSockets

logger :: Maybe DebugLogger
logger = Just $ DebugLogger pPrint

gameStream :: MarvelGameId -> WebSocketsT Handler ()
gameStream gameId = catchingConnectionException $ do
  writeChannel <- lift $ getChannel gameId
  gameChannelClients <- appGameChannelClients <$> getYesod
  atomicModifyIORef' gameChannelClients
    $ \channelClients -> (Map.insertWith (+) gameId 1 channelClients, ())
  bracket (liftIO $ atomically $ dupTChan writeChannel) closeConnection
    $ \readChannel -> race_
        (forever $ atomically (readTChan readChannel) >>= sendTextData)
        (runConduit $ sourceWS .| mapM_C (atomically . writeTChan writeChannel))
 where
  closeConnection _ = do
    gameChannelsRef <- appGameChannels <$> lift getYesod
    gameChannelClientsRef <- appGameChannelClients <$> lift getYesod
    clientCount <-
      atomicModifyIORef' gameChannelClientsRef $ \channelClients ->
        ( Map.adjust pred gameId channelClients
        , Map.findWithDefault 1 gameId channelClients - 1
        )
    when (clientCount == 0)
      $ atomicModifyIORef' gameChannelsRef
      $ \gameChannels' -> (Map.delete gameId gameChannels', ())

catchingConnectionException :: WebSocketsT Handler () -> WebSocketsT Handler ()
catchingConnectionException f =
  f `catch` \e -> $(logWarn) $ T.pack $ show (e :: ConnectionException)

data GetGameJson = GetGameJson
  { identityId :: Maybe IdentityId
  , multiplayerMode :: MultiplayerVariant
  , game :: ApiGame
  }
  deriving stock (Show, Generic)
  deriving anyclass ToJSON

getApiV1MarvelGameR :: MarvelGameId -> Handler GetGameJson
getApiV1MarvelGameR gameId = do
  webSockets (gameStream gameId)
  userId <- requireUserId
  ge <- runDB $ get404 gameId
  MarvelPlayer {..} <- runDB $ entityVal <$> getBy404
    (UniquePlayer userId gameId)
  let
    gameJson@Game {..} = marvelGameCurrentData ge
    investigatorId = case marvelGameMultiplayerVariant ge of
      Solo -> coerce gameActivePlayer
      WithFriends -> coerce marvelPlayerIdentityId

  gameRef <- newIORef gameJson
  queueRef <- newIORef []
  response <- runGameApp (GameApp gameRef queueRef Nothing) (toApiGame $ Entity gameId ge)

  pure $ GetGameJson
    (Just investigatorId)
    (marvelGameMultiplayerVariant ge)
    response

getApiV1MarvelGameSpectateR :: MarvelGameId -> Handler GetGameJson
getApiV1MarvelGameSpectateR gameId = do
  webSockets (gameStream gameId)
  ge <- runDB $ get404 gameId
  let
    gameJson@Game {..} = marvelGameCurrentData ge
    identityId = coerce gameActivePlayer

  gameRef <- newIORef gameJson
  queueRef <- newIORef []
  response <- runGameApp (GameApp gameRef queueRef Nothing) (toApiGame $ Entity gameId ge)
  pure $ GetGameJson
    (Just identityId)
    (marvelGameMultiplayerVariant ge)
    response

getApiV1MarvelGamesR :: Handler [ApiGame]
getApiV1MarvelGamesR = do
  userId <- requireUserId
  runDB $ selectMap toInactiveApiGame $ do
    (players :& games) <-
      from
      $ table @MarvelPlayer
      `InnerJoin` table @MarvelGame
      `on` (\(players :& games) ->
             players ^. MarvelPlayerMarvelGameId ==. games ^. persistIdField
           )
    where_ (players ^. MarvelPlayerUserId ==. val userId)
    pure games

selectMap
  :: (SqlSelect (SqlExpr a) a, MonadIO m)
  => (a -> b)
  -> SqlQuery (SqlExpr a)
  -> SqlPersistT m [b]
selectMap f q = map f <$> select q

data CreateGamePost = CreateGamePost
  { deckIds :: [Maybe MarvelDeckId]
  , playerCount :: Int
  , scenarioId :: CardCode
  , gameName :: Text
  , multiplayerVariant :: MultiplayerVariant
  }
  deriving stock (Show, Generic)
  deriving anyclass FromJSON

postApiV1MarvelGamesR :: Handler ApiGame
postApiV1MarvelGamesR = do
  userId <- requireUserId
  CreateGamePost {..} <- requireCheckJsonBody
  identities <- for (catMaybes deckIds) $ \deckId -> do
    deck <- runDB $ get404 deckId
    when (marvelDeckUserId deck /= userId) notFound
    (iid, decklist) <- liftIO $ loadDecklist deck
    liftIO $ initPlayer iid decklist
  let player = maybe (error "must have a player") head $ nonEmpty identities
  case lookupScenario scenarioId of
    Nothing -> error "invalid scenario"
    Just scenario -> do
      let g = newGame player scenario
      gameRef <- newIORef g
      queueRef <- newIORef [StartGame]
      runGameApp (GameApp gameRef queueRef Nothing) runGameMessages
      ge <- readIORef gameRef
      let
        diffUp = diff g ge
        diffDown = diff ge g
      updatedQueue <- readIORef queueRef
      let
        createdGame = MarvelGame
          gameName
          ge
          [Step diffUp diffDown updatedQueue]
          []
          multiplayerVariant
      gameId <- runDB $ do
        gameId <- insert createdGame
        insert_ $ MarvelPlayer userId gameId (coerce $ toId player)
        pure gameId
      runGameApp (GameApp gameRef queueRef Nothing) (toApiGame $ Entity gameId createdGame)

newtype Answer
  = Answer QuestionResponse
  deriving stock Generic

instance FromJSON Answer where
  parseJSON =
    genericParseJSON $ defaultOptions { tagSingleConstructors = True }

data QuestionResponse = QuestionResponse
  { qrChoice :: Int
  , qrIdentityId :: Maybe IdentityId
  }
  deriving stock Generic

newtype PaymentAmountsResponse = PaymentAmountsResponse
  { parAmounts :: HashMap IdentityId Int }
  deriving stock Generic

newtype AmountsResponse = AmountsResponse
  { arAmounts :: HashMap Text Int }
  deriving stock Generic

instance FromJSON QuestionResponse where
  parseJSON = genericParseJSON $ aesonOptions $ Just "qr"

instance FromJSON PaymentAmountsResponse where
  parseJSON = genericParseJSON $ aesonOptions $ Just "par"

instance FromJSON AmountsResponse where
  parseJSON = genericParseJSON $ aesonOptions $ Just "ar"

-- extract :: Int -> [a] -> (Maybe a, [a])
-- extract n xs =
--   let a = xs !!? n in (a, [ x | (i, x) <- zip [0 ..] xs, i /= n ])

putApiV1MarvelGameR :: MarvelGameId -> Handler ()
putApiV1MarvelGameR gameId = do
  userId <- requireUserId
  MarvelGame {..} <- runDB $ get404 gameId
  response <- requireCheckJsonBody
  Entity pid marvelPlayer <- runDB $ getBy404 (UniquePlayer userId gameId)
  let
    gameJson = marvelGameCurrentData
    identityId = fromMaybe
      (coerce $ marvelPlayerIdentityId marvelPlayer)
      (answerIdentity response)
  messages <- handleAnswer gameJson identityId response

  let currentQueue = maybe [] stepMessages $ head <$> nonEmpty marvelGameSteps

  gameRef <- newIORef gameJson
  queueRef <- newIORef (messages <> currentQueue)
  logRef <- newIORef []
  writeChannel <- getChannel gameId
  runGameApp (GameApp gameRef queueRef logger) runGameMessages
  ge <- readIORef gameRef
  let
    diffUp = diff marvelGameCurrentData ge
    diffDown = diff ge marvelGameCurrentData

  updatedQueue <- readIORef queueRef
  updatedLog <- (marvelGameLog <>) <$> readIORef logRef
  let
    updatedGame = MarvelGame
      marvelGameName
      ge
      (Step diffUp diffDown updatedQueue : marvelGameSteps)
      updatedLog
      marvelGameMultiplayerVariant

  void $ runDB $ do
    replace gameId updatedGame
    case marvelGameMultiplayerVariant of
      Solo -> replace pid $ marvelPlayer
        { marvelPlayerIdentityId = coerce (view activePlayerL ge)
        }
      WithFriends -> pure ()

  apiResponse <- runGameApp (GameApp gameRef queueRef Nothing) (toApiGame $ Entity gameId updatedGame)
  liftIO $ atomically $ writeTChan
    writeChannel
    (encode $ GameUpdate apiResponse)

newtype RawGameJsonPut = RawGameJsonPut
  { gameMessage :: Message
  }
  deriving stock (Show, Generic)
  deriving anyclass FromJSON

putApiV1MarvelGameRawR :: MarvelGameId -> Handler ()
putApiV1MarvelGameRawR gameId = do
  _ <- requireUserId
  MarvelGame {..} <- runDB $ get404 gameId
  response <- requireCheckJsonBody
  let
    gameJson = marvelGameCurrentData
    message = gameMessage response
  let currentQueue = maybe [] stepMessages $ head <$> nonEmpty marvelGameSteps
  gameRef <- newIORef gameJson
  queueRef <- newIORef (message : currentQueue)
  logRef <- newIORef []
  writeChannel <- getChannel gameId
  runGameApp (GameApp gameRef queueRef Nothing) runGameMessages
  ge <- readIORef gameRef
  updatedQueue <- readIORef queueRef
  let
    diffUp = diff marvelGameCurrentData ge
    diffDown = diff ge marvelGameCurrentData
  updatedLog <- (marvelGameLog <>) <$> readIORef logRef
  let
    updatedGame = MarvelGame
      marvelGameName
      ge
      (Step diffUp diffDown updatedQueue : marvelGameSteps)
      updatedLog
      marvelGameMultiplayerVariant
  apiResponse <- runGameApp (GameApp gameRef queueRef Nothing) (toApiGame $ Entity gameId updatedGame)
  liftIO $ atomically $ writeTChan
    writeChannel
    (encode $ GameUpdate apiResponse)
  void $ runDB $ replace gameId updatedGame

deleteApiV1MarvelGameR :: MarvelGameId -> Handler ()
deleteApiV1MarvelGameR gameId = void $ runDB $ do
  delete $ do
    players <- from $ table @MarvelPlayer
    where_ $ players ^. MarvelPlayerMarvelGameId ==. val gameId
  delete $ do
    games <- from $ table @MarvelGame
    where_ $ games ^. persistIdField ==. val gameId

answerIdentity :: Answer -> Maybe IdentityId
answerIdentity (Answer response) = qrIdentityId response

extract :: Int -> [a] -> (Maybe a, [a])
extract n xs =
  let a = xs !!? n in (a, [ x | (i, x) <- zip [0 ..] xs, i /= n ])

handleAnswer :: MonadIO m => Game -> IdentityId -> Answer -> m [Message]
handleAnswer g@Game {..} identityId = \case
  Answer response -> case HashMap.lookup identityId gameQuestion of
    Just (ChooseOne qs) -> case qs !!? qrChoice response of
      Nothing -> pure [Ask identityId $ ChooseOne qs]
      Just choice -> do
        gameRef <- newIORef g
        queueRef <- newIORef []
        runGameApp (GameApp gameRef queueRef Nothing)
          $ choiceMessages identityId choice
    Just (ChooseOneAtATime qs) -> case extract (qrChoice response) qs of
      (Nothing, msgs') -> pure [Ask identityId $ ChooseOneAtATime msgs']
      (Just choice, msgs') -> do
        gameRef <- newIORef g
        queueRef <- newIORef []
        results <- runGameApp (GameApp gameRef queueRef Nothing)
          $ choiceMessages identityId choice
        pure
          $ results
          <> [ Ask identityId $ ChooseOneAtATime msgs' | not (null msgs') ]
    _ -> error "Wrong question type"
