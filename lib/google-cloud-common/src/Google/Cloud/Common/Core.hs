{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}

{- |
Module      : Google.Cloud.Common.Core
License     : MIT
Maintainer  : maintainers@google-cloud-haskell
Stability   : experimental

Shared functionality and types used across Google Cloud client packages.

This module provides:

- Access token generation using either application credentials or metadata server
- A thin HTTP request layer with JSON helpers
- Shared request option types and utilities
- Convenience helpers for path building
-}
module Google.Cloud.Common.Core
  ( -- * Authentication
    genAccessToken

    -- * Request API
  , RequestOptions (..)
  , RequestMethod (..)
  , doRequest
  , doRequestJSON
  , doRequestRaw

    -- * Types
  , AccessTokenResp (..)
  , GoogleApplicationCred (..)

    -- * Utilities
  , toPath
  ) where

import Control.Exception (try)
import Data.Aeson
import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Lazy.Char8 as BSL
import Data.IORef
import qualified Data.Map.Strict as Map
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import qualified Data.Text.Encoding as TE
import qualified Data.Text.Lazy as T
import Data.Text.Lazy.Encoding (decodeUtf8)
import Data.Time.Clock.POSIX
import GHC.IO (unsafePerformIO)
import Network.HTTP.Simple
import System.Environment (lookupEnv)
import qualified Web.JWT as JWT

{- | Google application credential JSON as produced by a service account key.

This structure corresponds to the JSON file pointed to by
the @GOOGLE_APPLICATION_CREDENTIALS@ environment variable.
Field names match the JSON keys, with minor adaptations for Haskell naming.
-}
data GoogleApplicationCred = GooGleApplicationCred
  { type_ :: Text
  -- ^ Type of the credential (usually @service_account@)
  , projectId :: Text
  -- ^ Project ID associated with the credential
  , privateKeyId :: Text
  -- ^ Private key ID
  , privateKey :: Text
  -- ^ PEM-encoded RSA private key
  , clientEmail :: Text
  -- ^ Service account email address
  , clientId :: Text
  -- ^ Client ID
  , authUri :: Text
  -- ^ OAuth2 authorization URI
  , tokenUri :: Text
  -- ^ OAuth2 token exchange URI
  , auth_provider_x509_cert_url :: Text
  -- ^ Auth provider certificate URL
  , client_x509_cert_url :: Text
  -- ^ Client certificate URL
  , universe_domain :: Text
  -- ^ Universe domain
  }
  deriving (Eq, Show)

-- | Response returned by OAuth2 token endpoints.
data AccessTokenResp = AccessTokenResp
  { accessToken :: String
  -- ^ Bearer access token
  , expiresIn :: Integer
  -- ^ Lifetime in seconds
  , tokenType :: String
  -- ^ Token type (usually @Bearer@)
  }
  deriving (Eq, Show)

instance FromJSON AccessTokenResp where
  parseJSON = withObject "AccessTokenResp" $ \v ->
    AccessTokenResp
      <$> v .: "access_token"
      <*> v .: "expires_in"
      <*> v .: "token_type"

instance FromJSON GoogleApplicationCred where
  parseJSON = withObject "GoogleApplicationCred" $ \v ->
    GooGleApplicationCred
      <$> v .: "type"
      <*> v .: "project_id"
      <*> v .: "private_key_id"
      <*> v .: "private_key"
      <*> v .: "client_email"
      <*> v .: "client_id"
      <*> v .: "auth_uri"
      <*> v .: "token_uri"
      <*> v .: "auth_provider_x509_cert_url"
      <*> v .: "client_x509_cert_url"
      <*> v .: "universe_domain"

{- | Generate a Google Cloud access token.

Resolution order:

1. If @GOOGLE_APPLICATION_CREDENTIALS@ is set, read the JSON file and
   exchange a JWT for an OAuth2 access token.
2. Otherwise, fetch a token from the Compute metadata server (suitable for
   GCE/GKE environments with a default service account).

Returns 'Right' token on success, or 'Left' with an error message.
-}
genAccessToken :: IO (Either String BS.ByteString)
genAccessToken = do
  mbEnv <- lookupEnv "GOOGLE_APPLICATION_CREDENTIALS"
  case mbEnv of
    Just jsonFilePath -> do
      eRes <- readJSONFile jsonFilePath
      case eRes of
        Left err -> pure $ Left err
        Right jsonVal -> getValidToken jsonVal
    Nothing -> getTokenFromMetadataServer

-- | In-memory cache of the latest token and its expiry epoch seconds.
{-# NOINLINE tokenCache #-}
tokenCache :: IORef (Maybe (BS.ByteString, Integer))
tokenCache = unsafePerformIO (newIORef Nothing)

-- | Create a signed JWT for the given credential with Cloud Platform scope.
createJWT :: GoogleApplicationCred -> IO (Maybe Text)
createJWT GooGleApplicationCred {..} = do
  now <- round <$> getPOSIXTime :: IO Integer
  let expiration = now + 3600 -- 1 hour expiry
  let claims =
        mempty
          { JWT.iss = JWT.stringOrURI clientEmail
          , JWT.aud = Left <$> JWT.stringOrURI tokenUri
          , JWT.exp = JWT.numericDate (fromIntegral expiration)
          , JWT.iat = JWT.numericDate (fromIntegral now)
          , JWT.unregisteredClaims =
              JWT.ClaimsMap
                ( Map.singleton
                    "scope"
                    "https://www.googleapis.com/auth/cloud-platform"
                )
          }

      mbPrivateKeyBS = JWT.readRsaSecret (TE.encodeUtf8 privateKey)
      jwtHeader =
        JWT.JOSEHeader
          { JWT.alg = Just JWT.RS256
          , JWT.typ = Just "jwt"
          , JWT.kid = Just privateKeyId
          , JWT.cty = Nothing
          }
  case mbPrivateKeyBS of
    Nothing -> pure Nothing
    Just pKey ->
      return $
        Just $
          JWT.encodeSigned
            (JWT.EncodeRSAPrivateKey pKey)
            jwtHeader
            claims

-- | Exchange a JWT for an OAuth2 access token. Returns token bytes and expiry.
fetchAccessToken :: Text -> IO (Maybe (BS.ByteString, Integer))
fetchAccessToken jwt = do
  now <- round <$> getPOSIXTime
  let reqBody =
        "grant_type=urn%3Aietf%3Aparams%3Aoauth%3Agrant-type%3Ajwt-bearer&assertion=" <> jwt
  initialRequest <- parseRequest "https://oauth2.googleapis.com/token"
  let request =
        setRequestHeaders
          [("Content-Type", "application/x-www-form-urlencoded")]
          ( setRequestMethod
              "POST"
              (setRequestBodyLBS (BSL.fromStrict $ TE.encodeUtf8 reqBody) initialRequest)
          )
  response <- httpLbs request
  let rBody = eitherDecodeStrict (BS.toStrict $ getResponseBody response)
  case rBody of
    Right AccessTokenResp {..} -> return (Just (BS.pack accessToken, now + 3600))
    Left err -> do
      print err
      return Nothing

-- | Get a valid token, reusing a cached one if it has not expired yet.
getValidToken :: GoogleApplicationCred -> IO (Either String BS.ByteString)
getValidToken json_ = do
  now <- round <$> getPOSIXTime
  cached <- readIORef tokenCache
  case cached of
    Just (token, expiry) | now < expiry -> return (Right token)
    _ -> do
      maybeJwt <- createJWT json_
      case maybeJwt of
        Just jwt -> do
          maybeToken <- fetchAccessToken jwt
          case maybeToken of
            Just (token, expiry) -> do
              writeIORef tokenCache (Just (token, expiry))
              return (Right token)
            Nothing -> return $ Left "Failed to exchange JWT for access token"
        Nothing -> return $ Left "Failed to create JWT from credentials"

-- | Read and decode a JSON credentials file into 'GoogleApplicationCred'.
readJSONFile :: FilePath -> IO (Either String GoogleApplicationCred)
readJSONFile jsonFilePath = do
  eRes <- try $ readFile jsonFilePath
  case eRes of
    Left err -> pure . Left $ show (err :: IOError)
    Right content -> pure $ eitherDecodeStrict (BS.pack content)

-- | Fetch an access token from the metadata server (GCE/GKE default SA).
getTokenFromMetadataServer :: IO (Either String BS.ByteString)
getTokenFromMetadataServer = do
  initialRequest <-
    parseRequest
      "http://metadata.google.internal/computeMetadata/v1/instance/service-accounts/default/token"
  let request =
        setRequestHeaders
          [("Metadata-Flavor", "Google")]
          ( setRequestMethod
              "GET"
              initialRequest
          )
  eResp <- try $ httpLbs request
  case eResp of
    Left (_ :: HttpException) ->
      pure $
        Left
          "No application credentials found and metadata server is unavailable. Set GOOGLE_APPLICATION_CREDENTIALS to a service account JSON or run on GCE/GKE with a default service account."
    Right response -> do
      let rBody = eitherDecodeStrict (BS.toStrict $ getResponseBody response)
      case rBody of
        Right AccessTokenResp {..} -> return . Right $ BS.pack accessToken
        Left _ ->
          pure $ Left "Unexpected response from metadata server while fetching access token."

-- | Custom HTTP methods used in requests.
data RequestMethod = GET | POST | PATCH | DELETE | PUT
  deriving (Eq, Show)

{- | Options for performing an HTTP request.

You can construct this record and pass it to 'doRequest' or 'doRequestJSON'.
Unused optional fields can be 'Nothing'.
-}
data RequestOptions = RequestOptions
  { reqMethod :: RequestMethod
  -- ^ HTTP method to use
  , reqUrl :: String
  -- ^ Base URL (e.g. https://example.googleapis.com)
  , mbReqHeaders :: Maybe RequestHeaders
  -- ^ Optional request headers
  , mbQueryParams :: Maybe Query
  -- ^ Optional query string parameters
  , mbReqBody :: Maybe BSL.ByteString
  -- ^ Optional lazy bytestring request body
  , mbReqPath :: Maybe String
  -- ^ Optional path to append to 'reqUrl'
  }
  deriving (Eq, Show)

{- | Perform an HTTP request and return the raw 'Response' body on success.
Returns a textual error on failure.
-}
doRequestRaw :: RequestOptions -> IO (Either String (Response BSL.ByteString))
doRequestRaw RequestOptions {..} = do
  token <- genAccessToken
  case token of
    Left err -> pure (Left err)
    Right tokenBS -> do
      initialRequest <- parseRequest (reqUrl <> fromMaybe "" mbReqPath)
      let request =
            foldl
              (flip ($))
              initialRequest
              [ maybe Prelude.id setRequestHeaders mbReqHeaders
              , maybe Prelude.id setRequestQueryString mbQueryParams
              , maybe Prelude.id setRequestBodyLBS mbReqBody
              , addRequestHeader "Authorization" ("Bearer " <> tokenBS)
              , setRequestMethod (BS.pack $ show reqMethod)
              ]
      eResp <- try $ httpLbs request
      case eResp of
        Left err -> pure $ Left (show (err :: HttpException))
        Right resp -> pure $ Right resp

{- | Perform an HTTP request and return the response body.
Fails with an "HTTP <status>: <body>" error if the status is non-2xx.
-}
doRequest :: RequestOptions -> IO (Either String BSL.ByteString)
doRequest reqOpts = do
  eResp <- doRequestRaw reqOpts
  case eResp of
    Left err -> pure $ Left err
    Right resp -> do
      let respStatus = getResponseStatusCode resp
          respBody = getResponseBody resp
      if respStatus <= 299 && respStatus >= 200
        then return $ Right respBody
        else return $ Left ("HTTP " <> show respStatus <> ": " <> T.unpack (decodeUtf8 respBody))

{- | Perform an HTTP request and decode the response body as JSON.
An empty body is treated as @{}@ for convenience.
-}
doRequestJSON :: (FromJSON b) => RequestOptions -> IO (Either String b)
doRequestJSON reqOpts = do
  eResp <- doRequest reqOpts
  case eResp of
    Left err -> return $ Left err
    Right respBody -> do
      let respBody' = if BSL.null respBody then "{}" else respBody
      case eitherDecode respBody' of
        Left err -> pure $ Left err
        Right res -> pure $ Right res

{- | Build a URL path from path segments.

Example:

>>> toPath ["hello", "world"]
"/hello/world"
-}
toPath :: [String] -> String
toPath = foldl (\acc x -> acc <> "/" <> x) ""
