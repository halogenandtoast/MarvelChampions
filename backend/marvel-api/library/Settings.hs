{-# LANGUAGE CPP #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
module Settings where

import Prelude

import Data.String (fromString)
import Control.Monad (unless)
import Data.Text (Text)
import Control.Exception qualified as Exception
import Data.Aeson
  (FromJSON(..), Result(..), Value, fromJSON, withObject, (.!=), (.:), (.:?))
import Data.ByteString.Char8 qualified as BS8
import Data.FileEmbed (embedFile)
import Data.String.Conversions.Monomorphic (toStrictByteString)
import Data.Yaml (decodeEither')
import Data.Yaml.Config (applyEnvValue)
import Database.Persist.Postgresql (PostgresConf(..))
import Network.Wai.Handler.Warp (HostPreference)
import URI.ByteString
  ( Authority(..)
  , Host(..)
  , Port(..)
  , Scheme(..)
  , URIRef(..)
  , UserInfo(..)
  , parseURI
  , strictURIParserOptions
  )

abortLeft :: (MonadFail m, Show e) => Either e b -> m b
abortLeft = either (fail . ("DATABASE_URL failed to parse: " <>) . show) pure

abortNothing :: MonadFail m => String -> Maybe a -> m a
abortNothing s = maybe (fail $ "DATABASE_URL is missing " <> s) pure

fromDatabaseUrl :: (MonadFail m) => Int -> Text -> m PostgresConf
fromDatabaseUrl size url = do
  uri <- abortLeft $ parseURI strictURIParserOptions $ toStrictByteString url
  auth <- abortNothing "authority" $ uriAuthority uri
  port <- abortNothing "port" $ authorityPort auth
  dbName <- abortNothing "path" $ snd <$> BS8.uncons (uriPath uri)
  unless (schemeBS (uriScheme uri) == "postgres") $ fail "DATABASE_URL has unknown scheme"
  pure PostgresConf
    { pgConnStr =
        maybe "" (\info -> "user=" <> uiUsername info <> " password=" <> uiPassword info) (authorityUserInfo auth)
        <> " host=" <> hostBS (authorityHost auth)
        <> " port=" <> BS8.pack (show $ portNumber port)
        <> " dbname=" <> dbName
    , pgPoolSize = size
    , pgPoolStripes = 1
    , pgPoolIdleTimeout = 60
    }

-- | Runtime settings to configure this application. These settings can be
-- loaded from various sources: defaults, environment variables, config files,
-- theoretically even a database.
data AppSettings = AppSettings
    { appDatabaseConf           :: PostgresConf
    -- ^ Configuration settings for accessing the database.
    , appRoot                   :: Maybe Text
    -- ^ Base for all generated URLs. If @Nothing@, determined
    -- from the request headers.
    , appHost                   :: HostPreference
    -- ^ Host/interface the server should bind to.
    , appPort                   :: Int
    -- ^ Port to listen on
    , appIpFromHeader           :: Bool
    -- ^ Get the IP address from the header when logging. Useful when sitting
    -- behind a reverse proxy.

    , appDetailedRequestLogging :: Bool
    -- ^ Use detailed request logging system
    , appShouldLogAll           :: Bool
    -- ^ Should all log messages be displayed?
    , appReloadTemplates        :: Bool
    -- ^ Use the reload version of templates
    , appSkipCombining          :: Bool
    -- ^ Perform no stylesheet/script combining
    , appJwtSecret :: Text
    }

instance FromJSON AppSettings where
    parseJSON = withObject "AppSettings" $ \o -> do
        let defaultDev =
#ifdef DEVELOPMENT
                True
#else
                False
#endif
        poolSize                  <- o .: "database-pool-size"
        appDatabaseConf           <- fromDatabaseUrl poolSize =<< (o .: "database-url")
        appRoot                   <- o .:? "approot"
        appHost                   <- fromString <$> o .: "host"
        appPort                   <- o .: "port"
        appIpFromHeader           <- o .: "ip-from-header"

        dev                       <- o .:? "development"      .!= defaultDev

        appDetailedRequestLogging <- o .:? "detailed-logging" .!= dev
        appShouldLogAll           <- o .:? "should-log-all"   .!= dev
        appReloadTemplates        <- o .:? "reload-templates" .!= dev
        appSkipCombining          <- o .:? "skip-combining"   .!= dev
        appJwtSecret <- o .: "jwt-secret"

        pure AppSettings {..}

-- | Raw bytes at compile time of @config/settings.yml@
configSettingsYmlBS :: BS8.ByteString
configSettingsYmlBS = $(embedFile "config/settings.yml")

-- | @config/settings.yml@, parsed to a @Value@.
configSettingsYmlValue :: Value
configSettingsYmlValue = either Exception.throw id
                       $ decodeEither' configSettingsYmlBS

-- | A version of @AppSettings@ parsed at compile time from @config/settings.yml@.
compileTimeAppSettings :: AppSettings
compileTimeAppSettings =
    case fromJSON $ applyEnvValue False mempty configSettingsYmlValue of
        Error e -> error e
        Success settings -> settings
