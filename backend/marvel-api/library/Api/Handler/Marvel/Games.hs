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
import qualified Data.Map.Strict as Map
import qualified Data.Text as T
import Data.Traversable (for)
import Database.Esqueleto.Experimental hiding (update)
import Json
import Marvel.Card.Code
import Marvel.Entity
import Marvel.Game
import Marvel.Id
import Marvel.Message
import Marvel.Scenario
import Network.WebSockets (ConnectionException)
import UnliftIO.Exception (bracket, catch)
import Yesod.WebSockets

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
  , game :: MarvelGame
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
    Game {..} = marvelGameCurrentData ge
    investigatorId = case marvelGameMultiplayerVariant ge of
      Solo -> coerce gameActivePlayer
      WithFriends -> coerce marvelPlayerIdentityId
  pure $ GetGameJson (Just investigatorId) (marvelGameMultiplayerVariant ge) ge

getApiV1MarvelGameSpectateR :: MarvelGameId -> Handler GetGameJson
getApiV1MarvelGameSpectateR gameId = do
  webSockets (gameStream gameId)
  ge <- runDB $ get404 gameId
  let
    Game {..} = marvelGameCurrentData ge
    identityId = coerce gameActivePlayer
  pure $ GetGameJson (Just identityId) (marvelGameMultiplayerVariant ge) ge

getApiV1MarvelGamesR :: Handler [MarvelGame]
getApiV1MarvelGamesR = do
  userId <- requireUserId
  games <- runDB $ select $ do
    (players :& games) <-
      from
      $ table @MarvelPlayer
      `InnerJoin` table @MarvelGame
      `on` (\(players :& games) ->
             players ^. MarvelPlayerMarvelGameId ==. games ^. persistIdField
           )
    where_ (players ^. MarvelPlayerUserId ==. val userId)
    pure games
  pure $ map entityVal games

data CreateGamePost = CreateGamePost
  { deckIds :: [Maybe MarvelDeckId]
  , playerCount :: Int
  , scenarioId :: CardCode
  , gameName :: Text
  , multiplayerVariant :: MultiplayerVariant
  }
  deriving stock (Show, Generic)
  deriving anyclass FromJSON

postApiV1MarvelGamesR :: Handler MarvelGame
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
      runDB $ do
        gameId <- insert $ MarvelGame
          gameName
          ge
          [Step diffUp diffDown updatedQueue]
          []
          multiplayerVariant
        insert_ $ MarvelPlayer userId gameId (coerce $ toId player)
      pure $ MarvelGame
        gameName
        ge
        [Step diffUp diffDown updatedQueue]
        []
        multiplayerVariant

newtype Answer
  = Answer QuestionResponse
  deriving stock Generic
  deriving anyclass FromJSON

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
  -- response <- requireCheckJsonBody
  Entity pid marvelPlayer <- runDB $ getBy404 (UniquePlayer userId gameId)
  let
    gameJson = marvelGameCurrentData
    -- investigatorId =
    --   fromMaybe (coerce marvelPlayerIdentityId) (answerIdentity response)
    messages = [] -- handleAnswer gameJson investigatorId response

  let currentQueue = maybe [] stepMessages $ head <$> nonEmpty marvelGameSteps

  gameRef <- newIORef gameJson
  queueRef <- newIORef (messages <> currentQueue)
  logRef <- newIORef []
  writeChannel <- getChannel gameId
  runGameApp (GameApp gameRef queueRef Nothing) runGameMessages
  ge <- readIORef gameRef
  let
    diffUp = diff marvelGameCurrentData ge
    diffDown = diff ge marvelGameCurrentData

  updatedQueue <- readIORef queueRef
  updatedLog <- (marvelGameLog <>) <$> readIORef logRef
  void $ runDB $ do
    replace gameId $ MarvelGame
      marvelGameName
      ge
      (Step diffUp diffDown updatedQueue : marvelGameSteps)
      updatedLog
      marvelGameMultiplayerVariant
    case marvelGameMultiplayerVariant of
      Solo -> replace pid $ marvelPlayer
        { marvelPlayerIdentityId = coerce (view activePlayerL ge)
        }
      WithFriends -> pure ()

  liftIO $ atomically $ writeTChan writeChannel (encode $ GameUpdate ge)

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
  liftIO $ atomically $ writeTChan writeChannel (encode $ GameUpdate ge)
  void $ runDB
    (replace
      gameId
      (MarvelGame
        marvelGameName
        ge
        (Step diffUp diffDown updatedQueue : marvelGameSteps)
        updatedLog
        marvelGameMultiplayerVariant
      )
    )

deleteApiV1MarvelGameR :: MarvelGameId -> Handler ()
deleteApiV1MarvelGameR gameId = void $ runDB $ do
  delete $ do
    players <- from $ table @MarvelPlayer
    where_ $ players ^. MarvelPlayerMarvelGameId ==. val gameId
  delete $ do
    games <- from $ table @MarvelGame
    where_ $ games ^. persistIdField ==. val gameId

-- answerIdentity :: Answer -> Maybe IdentityId
-- answerIdentity (Answer response) = qrIdentityId response

-- handleAnswer :: Game -> IdentityId -> Answer -> [Message]
-- handleAnswer Game {..} identityId = \case
--   Answer response -> case HashMap.lookup identityId gameQuestion of
--     Just (ChooseOne qs) -> case qs !!? qrChoice response of
--       Nothing -> [Ask identityId $ ChooseOne qs]
--       Just msg -> [msg]
--     _ -> error "Wrong question type"
