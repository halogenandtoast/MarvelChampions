module Api.Handler.Marvel.PendingGames (
  putApiV1MarvelPendingGameR,
) where

import Import hiding ((==.))

import Api.Marvel.Helpers
import Control.Monad (when)
import Data.Coerce (coerce)
import Data.IORef
import Data.List.NonEmpty (nonEmpty)
import Data.List.NonEmpty qualified as NE
import GHC.Generics (Generic)
import Marvel.Debug
import Marvel.Entity
import Marvel.Game
import Marvel.Id
import Text.Pretty.Simple

logger :: Maybe DebugLogger
logger = Just $ DebugLogger pPrint

newtype JoinGameJson = JoinGameJson {deckId :: MarvelDeckId}
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

  let currentQueue = maybe [] stepMessages $ NE.head <$> nonEmpty marvelGameSteps

  gameRef <- liftIO $ newIORef marvelGameCurrentData
  queueRef <- liftIO $ newIORef currentQueue
  runGameApp (GameApp gameRef queueRef Nothing) $ do
    addPlayer playerIdentity
    runGameMessages

  updatedGame <- liftIO $ readIORef gameRef
  updatedQueue <- liftIO $ readIORef queueRef
  let updatedMessages = []

  let diffUp = diff marvelGameCurrentData updatedGame
      diffDown = diff updatedGame marvelGameCurrentData
      game' =
        MarvelGame
          marvelGameName
          updatedGame
          (Step diffUp diffDown updatedQueue : marvelGameSteps)
          updatedMessages
          marvelGameMultiplayerVariant

  apiResponse <- runGameApp (GameApp gameRef queueRef logger) (toApiGame $ Entity gameId game')

  sendRoom gameId $ GameUpdate apiResponse

  runDB $ replace gameId game'

  pure $
    MarvelGame
      marvelGameName
      updatedGame
      (Step diffUp diffDown updatedQueue : marvelGameSteps)
      updatedMessages
      marvelGameMultiplayerVariant
