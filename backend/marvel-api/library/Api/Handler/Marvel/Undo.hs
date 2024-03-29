module Api.Handler.Marvel.Undo (
  putApiV1MarvelGameUndoR,
) where

import Import hiding (delete, (==.))

import Api.Marvel.Helpers
import Control.Lens (view)
import Data.Coerce (coerce)
import Data.IORef
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
    step : remaining ->
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

          gameRef <- liftIO $ newIORef ge
          queueRef <- liftIO $ newIORef []
          apiResponse <- runGameApp (GameApp gameRef queueRef logger) (toApiGame $ Entity gameId game')
          sendRoom gameId (GameUpdate apiResponse)
          runDB $ do
            replace gameId game'
            replace pid $
              marvelPlayer
                { marvelPlayerIdentityId = coerce (view activePlayerL ge)
                }
