module Api.Handler.Marvel.PendingGames
  ( putApiV1MarvelPendingGameR
  ) where

import Import hiding (on, (==.))

import Api.Marvel.Helpers
import Control.Concurrent.STM.TChan
import Data.Aeson
import Marvel.Entity
import Marvel.Game
import Marvel.Id

newtype JoinGameJson = JoinGameJson { deckId :: MarvelDeckId }
  deriving stock (Show, Generic)
  deriving anyclass (FromJSON)

putApiV1MarvelPendingGameR :: MarvelGameId -> Handler MarvelGame
putApiV1MarvelPendingGameR gameId = do
  userId <- requireUserId
  JoinGameJson {..} <- requireCheckJsonBody
  MarvelGame {..} <- runDB $ get404 gameId

  deck <- runDB $ get404 deckId
  when (marvelDeckUserId deck /= userId) notFound
  playerIdentity <- liftIO $ uncurry initPlayer =<< loadDecklist deck
  runDB $ insert_ $ MarvelPlayer userId gameId (coerce $ toId playerIdentity)

  let currentQueue = maybe [] stepMessages $ head <$> nonEmpty marvelGameSteps

  gameRef <- newIORef marvelGameCurrentData
  queueRef <- newIORef currentQueue
  runGameApp (GameApp gameRef queueRef Nothing) $ do
    addPlayer playerIdentity
    runGameMessages

  updatedGame <- readIORef gameRef
  updatedQueue <- readIORef queueRef
  let updatedMessages = []

  let
    diffUp = diff marvelGameCurrentData updatedGame
    diffDown = diff updatedGame marvelGameCurrentData

  writeChannel <- getChannel gameId
  liftIO $ atomically $ writeTChan writeChannel $ encode $ GameUpdate
    updatedGame

  runDB $ replace gameId $ MarvelGame
    marvelGameName
    updatedGame
    (Step diffUp diffDown updatedQueue : marvelGameSteps)
    updatedMessages
    marvelGameMultiplayerVariant

  pure $ MarvelGame
    marvelGameName
    updatedGame
    (Step diffUp diffDown updatedQueue : marvelGameSteps)
    updatedMessages
    marvelGameMultiplayerVariant
