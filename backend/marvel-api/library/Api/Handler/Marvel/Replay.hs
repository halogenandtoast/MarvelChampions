module Api.Handler.Marvel.Replay (
  getApiV1MarvelGameReplayR,
) where

import Data.IORef
import GHC.Generics (Generic)
import Import hiding (delete, (==.))
import Marvel.Game

data GetReplayJson = GetReplayJson
  { totalSteps :: Int
  , game :: MarvelGame
  }
  deriving stock (Show, Generic)
  deriving anyclass (ToJSON)

newtype ReplayId = ReplayId {id :: MarvelGameId}
  deriving stock (Show, Generic)
  deriving anyclass (ToJSON)

getApiV1MarvelGameReplayR :: MarvelGameId -> Int -> Handler GetReplayJson
getApiV1MarvelGameReplayR gameId step = do
  _ <- requireUserId
  ge <- runDB $ get404 gameId
  let gameJson = marvelGameCurrentData ge
  let steps = reverse (take step (reverse $ marvelGameSteps ge))

  gameRef <- liftIO $ newIORef gameJson
  queueRef <- liftIO $ newIORef []

  runGameApp
    (GameApp gameRef queueRef Nothing)
    (replayChoices $ map stepPatchUp steps)

  ge' <- liftIO $ readIORef gameRef
  pure $
    GetReplayJson
      (length steps)
      ( MarvelGame
          (marvelGameName ge)
          ge'
          (marvelGameSteps ge)
          []
          (marvelGameMultiplayerVariant ge)
      )
