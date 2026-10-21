module Auth (signToken, verifyToken, hashPassword, checkPassword, lookupToken) where

import Control.Lens ((?~))
import Control.Monad.Time (MonadTime, currentTime)
import Crypto.BCrypt (fastBcryptHashingPolicy, hashPasswordUsingPolicy, validatePassword)
import Crypto.JWT
import Data.Text qualified as T
import AppPrelude hiding ((?~))
import Control.Lens (to, view)
import Yesod.Core (MonadHandler, lookupGetParam, lookupHeader)

newtype Claims = Claims {jwtClaims :: ClaimsSet}

instance HasClaimsSet Claims where
  claimsSet f s = fmap (\c -> s {jwtClaims = c}) (f (jwtClaims s))

instance FromJSON Claims where
  parseJSON = fmap Claims . parseJSON

instance ToJSON Claims where
  toJSON = toJSON . jwtClaims

key :: Text -> JWK
key = fromOctets . encodeUtf8

-- | A token naming the user it was issued to.
signToken :: (MonadIO m, MonadTime m) => Text -> Text -> m Text
signToken secret subject = do
  now <- currentTime
  let claims =
        Claims
          $ emptyClaimsSet
          & (claimIss ?~ "marvelchampions")
          & (claimSub ?~ fromString (unpack subject))
          & (claimIat ?~ NumericDate now)
  res <- liftIO $ runJOSE $ signJWT (key secret) (newJWSHeaderProtected HS256) claims
  case res of
    Left (err :: JWTError) -> error $ show err
    Right signed -> pure $ decodeUtf8 $ toStrict $ encodeCompact signed

-- | The subject of a valid token. A bad or stale token is simply not signed in.
verifyToken :: (MonadIO m) => Text -> Text -> m (Maybe Text)
verifyToken secret token = do
  res <- liftIO $ runJOSE do
    jwt <- decodeCompact $ fromStrict $ encodeUtf8 token
    verifyJWT (defaultJWTValidationSettings (== "marvelchampions")) (key secret) (jwt :: SignedJWT)
  pure $ case res of
    Left (_ :: JWTError) -> Nothing
    Right (claims :: Claims) -> view (claimSub . to (fmap (view string))) (jwtClaims claims)

-- | The token from an @Authorization: Token ...@ header, or a @token@ parameter for websockets.
lookupToken :: (MonadHandler m) => m (Maybe Text)
lookupToken = lookupHeader "Authorization" >>= maybe (lookupGetParam "token") (pure . extract . decodeUtf8)
 where
  extract value = case T.breakOn " " value of
    (scheme, rest) | T.toLower scheme == "token" -> Just (T.strip rest)
    _ -> Nothing

hashPassword :: Text -> IO (Maybe Text)
hashPassword password = fmap decodeUtf8 <$> hashPasswordUsingPolicy fastBcryptHashingPolicy (encodeUtf8 password)

checkPassword :: Text -> Text -> Bool
checkPassword passwordDigest password = validatePassword (encodeUtf8 passwordDigest) (encodeUtf8 password)
