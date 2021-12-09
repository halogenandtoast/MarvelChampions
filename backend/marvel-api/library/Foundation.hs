{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -Wno-missing-deriving-strategies #-}
{-# OPTIONS_GHC -Wno-redundant-constraints #-}

module Foundation where

import Import.NoFoundation

import Auth.JWT qualified as JWT
import Control.Concurrent.STM.TChan (TChan)
import Data.Aeson (Result(Success), fromJSON)
import Data.ByteString.Lazy qualified as BSL
import Network.HTTP.Client (Manager)
import Yesod.Core.Types (Logger)
import Yesod.Core.Unsafe qualified as Unsafe

import Orphans ()

data App = App
    { appSettings :: AppSettings
    , -- | Database connection pool.
      appConnPool :: ConnectionPool
    , appHttpManager :: Manager
    , appLogger :: Logger
    , appGameChannels :: IORef (Map MarvelGameId (TChan BSL.ByteString))
    , appGameChannelClients :: IORef (Map MarvelGameId Int)
    }

mkYesodData "App" $(parseRoutesFile "config/routes")

type DB a = forall (m :: Type -> Type). (MonadIO m) => ReaderT SqlBackend m a

instance Yesod App

unsafeHandler :: App -> Handler a -> IO a
unsafeHandler = Unsafe.fakeHandlerGetLogger appLogger

-- How to run database actions.
instance YesodPersist App where
    type YesodPersistBackend App = SqlBackend
    runDB action = do
        master <- getYesod
        runSqlPool action $ appConnPool master

userIdToToken :: UserId -> HandlerFor App Text
userIdToToken userId = do
    jwtSecret <- getJwtSecret
    pure $ JWT.jsonToToken jwtSecret $ toJSON userId

tokenToUserId :: Text -> Handler (Maybe UserId)
tokenToUserId token = do
    jwtSecret <- getJwtSecret
    let mUserId = fromJSON <$> JWT.tokenToJson jwtSecret token
    case mUserId of
        Just (Success userId) -> pure $ Just userId
        _ -> pure Nothing

getJwtSecret :: HandlerFor App Text
getJwtSecret = getsYesod $ appJwtSecret . appSettings

getRequestUserId :: Handler (Maybe UserId)
getRequestUserId = do
    mToken <- JWT.lookupToken
    liftHandler $ maybe (pure Nothing) tokenToUserId mToken

requireUserId :: Handler UserId
requireUserId = do
    mUserId <- getRequestUserId
    maybe notAuthenticated pure mUserId
