-- | Where a filed bug's game export goes: a content-addressed object in the bug
-- bucket, whose URL the reporter pastes into the GitHub issue. Arkham does this with
-- amazonka; one signed PUT is all we need, so it is signed here by hand (SigV4).
module BugUpload (uploadBugExport) where

import Crypto.Hash (Digest, SHA256, hash)
import Crypto.MAC.HMAC (HMAC, hmac)
import Data.ByteArray (convert)
import Data.ByteString.Base16 qualified as B16
import AppPrelude hiding (hash)
import Network.HTTP.Client (Manager, RequestBody (..), httpLbs, method, parseRequest, requestBody, requestHeaders, responseBody, responseStatus)
import Network.HTTP.Types.Status (statusIsSuccessful)
import System.Environment (lookupEnv)

bucket, region :: Text
bucket = "marvel-champions-bugs"
region = "us-east-1"

-- | Upload the export under @exports/<sha256>.json@ and give back its public URL.
-- Uploading the same export twice writes the same object, so there is no check first.
-- Credentials come from @AWS_ACCESS_KEY_ID@ and @AWS_SECRET_ACCESS_KEY@.
uploadBugExport :: Manager -> LByteString -> IO (Either Text Text)
uploadBugExport manager body = do
  creds <- liftA2 (,) <$> lookupEnv "AWS_ACCESS_KEY_ID" <*> lookupEnv "AWS_SECRET_ACCESS_KEY"
  case creds of
    Nothing -> pure $ Left "bug uploads are not set up: AWS_ACCESS_KEY_ID and AWS_SECRET_ACCESS_KEY are missing"
    Just (keyId, secret) -> do
      now <- getCurrentTime
      let payload = toStrict body
          payloadHash = hexHash payload
          host = bucket <> ".s3.amazonaws.com"
          path = "/exports/" <> decodeUtf8 payloadHash <> ".json"
          amzDate = formatTime defaultTimeLocale "%Y%m%dT%H%M%SZ" now
          day = take 8 amzDate
          scope = intercalate "/" [pack day, region, "s3", "aws4_request"]
          signedHeaders = "host;x-amz-content-sha256;x-amz-date"
          canonical =
            intercalate
              "\n"
              [ "PUT"
              , encodeUtf8 path
              , ""
              , "host:" <> encodeUtf8 host
              , "x-amz-content-sha256:" <> payloadHash
              , "x-amz-date:" <> encodeUtf8 (pack amzDate)
              , ""
              , signedHeaders
              , payloadHash
              ]
          toSign = intercalate "\n" ["AWS4-HMAC-SHA256", encodeUtf8 (pack amzDate), encodeUtf8 scope, hexHash canonical]
          signingKey = foldl' mac ("AWS4" <> encodeUtf8 (pack secret)) [encodeUtf8 (pack day), encodeUtf8 region, "s3", "aws4_request"]
          signature = B16.encode (mac signingKey toSign)
          authorization =
            "AWS4-HMAC-SHA256 Credential="
              <> encodeUtf8 (pack keyId)
              <> "/"
              <> encodeUtf8 scope
              <> ", SignedHeaders="
              <> signedHeaders
              <> ", Signature="
              <> signature
          url = "https://" <> host <> path
      request <- parseRequest (unpack url)
      response <-
        httpLbs
          request
            { method = "PUT"
            , requestBody = RequestBodyBS payload
            , requestHeaders =
                [ ("authorization", authorization)
                , ("content-type", "application/json")
                , ("x-amz-content-sha256", payloadHash)
                , ("x-amz-date", encodeUtf8 (pack amzDate))
                ]
            }
          manager
      pure
        if statusIsSuccessful (responseStatus response)
          then Right url
          else Left $ "could not upload the bug export: " <> decodeUtf8 (toStrict $ responseBody response)
 where
  hexHash :: ByteString -> ByteString
  hexHash bs = B16.encode (convert (hash bs :: Digest SHA256))
  mac :: ByteString -> ByteString -> ByteString
  mac key msg = convert (hmac key msg :: HMAC SHA256)
