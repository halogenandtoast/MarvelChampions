module Api.Handler.Marvel.Undo (
  putApiV1MarvelGameUndoR,
) where

import Import hiding (delete, on, (==.))

import Api.Marvel.Helpers
import Control.Concurrent.STM.TChan
import Control.Lens (view)
import Json
import Marvel.Debug
import Marvel.Game
import Marvel.Id
import Text.Pretty.Simple

logger :: Maybe DebugLogger
logger = Just $ DebugLogger pPrint

putApiV1MarvelGameUndoR :: MarvelGameId -> Handler ()
putApiV1MarvelGameUndoR gameId = do
  userId <- requireUserId
  MarvelGame {..} <- runDB $ get404 gameId
  Entity pid marvelPlayer <- runDB $ getBy404 (UniquePlayer userId gameId)

  case marvelGameSteps of
    [] -> pure ()
    [_] -> pure () -- can't undo the initial change
    step : remaining -> do
      writeChannel <- getChannel gameId

      case patch marvelGameCurrentData (stepPatchDown step) of
        Error e -> error $ show e
        Success ge -> do
          let game' =
                MarvelGame
                  marvelGameName
                  ge
                  remaining
                  marvelGameLog
                  marvelGameMultiplayerVariant

          gameRef <- newIORef ge
          queueRef <- newIORef []
          apiResponse <- runGameApp (GameApp gameRef queueRef logger) (toApiGame $ Entity gameId game')
          liftIO $
            atomically $
              writeTChan
                writeChannel
                (encode $ GameUpdate apiResponse)
          runDB $ do
            replace gameId game'
            replace pid $
              marvelPlayer
                { marvelPlayerIdentityId = coerce (view activePlayerL ge)
                }
