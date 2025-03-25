{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

{-|
Google.Cloud.Common.Core . This module contains function and types which are being shared 
among all GCP client packages.
|-}

module Google.Cloud.Common.Core
  ( genAccessToken
  , RequestOptions (..)
  , RequestMethod (..)
  , AccessTokenResp (..)
  , GoogleApplicationCred (..)
  , doRequest
  , doRequestJSON
  , toPath 
  ) where

import Control.Exception (try)
import Control.Monad.Error.Class (MonadError (throwError))
import Data.Aeson
import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Lazy.Char8 as BSL
import Data.IORef
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import qualified Data.Text.Encoding as TE
import qualified Data.Text.Lazy as T
import Data.Text.Lazy.Encoding (decodeUtf8)
import Data.Time.Clock.POSIX
import GHC.IO (unsafePerformIO)
import Network.HTTP.Simple
import System.Environment (lookupEnv)
import System.Process
import qualified Web.JWT as JWT
import qualified Data.Map.Strict as Map

data GoogleApplicationCred = GooGleApplicationCred
  { type_ :: Text
  , projectId :: Text
  , privateKeyId :: Text
  , privateKey :: Text
  , clientEmail :: Text
  , clientId :: Text
  , authUri :: Text
  , tokenUri :: Text
  , auth_provider_x509_cert_url :: Text
  , client_x509_cert_url :: Text
  , universe_domain :: Text
  }
  deriving (Eq, Show)

data AccessTokenResp = AccessTokenResp
  { accessToken :: String
  , expiresIn :: Integer
  , tokenType :: String
  }
  deriving (Eq, Show)

instance FromJSON AccessTokenResp where
  parseJSON (Object v) =
    AccessTokenResp
      <$> v .: "access_token"
      <*> v .: "expires_in"
      <*> v .: "token_type"
  parseJSON _ = error "asd"

instance FromJSON GoogleApplicationCred where
  parseJSON (Object v) =
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
  parseJSON _ = error "asd"

{- | Authentication
 - There are two ways of authentication,
    1. Application will look for `GOOGLE_APPLICATION_CREDENTIALS` environment variable
        and read the JSON file path.
    2. If the env variable is not stored, it will use `print-access-token`.
        Make sure gcloud is installed and logged in for this to work.
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
    Nothing -> genTokenViaPrintAccessToken

-- Global IORef to store the token in memory
{-# NOINLINE tokenCache #-}
tokenCache :: IORef (Maybe (BS.ByteString, Integer))
tokenCache = unsafePerformIO (newIORef Nothing)

-- Create JWT Token
createJWT :: GoogleApplicationCred -> IO (Maybe Text)
createJWT GooGleApplicationCred {..} = do
  now <- round <$> getPOSIXTime :: IO Integer
  let expiration = now + 3600 -- 1-hour expiry
  let claims =
        mempty
          { JWT.iss = JWT.stringOrURI clientEmail
          , JWT.aud = Left <$> JWT.stringOrURI tokenUri
          , JWT.exp = JWT.numericDate (fromIntegral expiration)
          , JWT.iat = JWT.numericDate (fromIntegral now)
          , JWT.unregisteredClaims = 
                JWT.ClaimsMap (Map.singleton "scope" "https://www.googleapis.com/auth/cloud-platform")
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

-- Exchange JWT for OAuth2 Access Token
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
  print response
  let rBody = eitherDecodeStrict (BS.toStrict $ getResponseBody response) 
  case rBody of
    Right AccessTokenResp {..} -> return (Just (BS.pack accessToken, now + 3600)) -- Expiry set to 1 hour
    Left err -> do 
        print err
        return Nothing

-- Get a valid token (reuses existing one if valid)
getValidToken :: GoogleApplicationCred -> IO (Either String BS.ByteString)
getValidToken json_ = do
  now <- round <$> getPOSIXTime
  cached <- readIORef tokenCache
  case cached of
    Just (token, expiry) | now < expiry -> return (Right token) -- Return cached token
    _ -> do
      maybeJwt <- createJWT json_
      case maybeJwt of
        Just jwt -> do
          maybeToken <- fetchAccessToken jwt
          case maybeToken of
            Just (token, expiry) -> do
              writeIORef tokenCache (Just (token, expiry)) -- Cache new token
              return (Right token)
            Nothing -> return $ Left "Something went wrong with token 1"
        Nothing -> return $ Left "Something went wrong with token 2"

readJSONFile :: FilePath -> IO (Either String GoogleApplicationCred)
readJSONFile jsonFilePath = do
  eRes <- try $ readFile jsonFilePath
  case eRes of
    Left err -> pure . Left $ show (err :: IOError)
    Right content -> pure $ eitherDecodeStrict (BS.pack content)

-- | Generate temporary access token using print-access-token command
genTokenViaPrintAccessToken :: IO (Either String BS.ByteString)
genTokenViaPrintAccessToken = do
  eRes <- try $ readProcess "gcloud" (words "auth print-access-token") []
  case eRes of
    Right s -> pure . Right $ BS.pack (init s)
    Left err -> pure . Left $ show (err :: IOError)

-- | Custom Request methods for convenience
data RequestMethod = GET | POST | PATCH | DELETE
  deriving (Eq, Show)

-- | A type that holds all required information for performing a network action
data RequestOptions = RequestOptions
  { reqMethod :: RequestMethod
  , reqUrl :: String
  , mbReqHeaders :: Maybe RequestHeaders
  , mbQueryParams :: Maybe Query
  , mbReqBody :: Maybe BSL.ByteString
  , mbReqPath :: Maybe String
  }
  deriving (Eq, Show)

-- | Helper function to perform network action for given request options
doRequest :: RequestOptions -> IO (Either String BSL.ByteString)
doRequest RequestOptions {..} = do
  token <- genAccessToken
  case token of
    Left err -> throwError (userError err)
    Right tokenBS -> do
      initialRequest <- parseRequest (reqUrl <> fromMaybe "" mbReqPath)
      let request =
            foldl
              (flip ($))
              initialRequest
              [ 
               maybe Prelude.id setRequestHeaders mbReqHeaders
              , maybe Prelude.id setRequestQueryString mbQueryParams
              , maybe Prelude.id setRequestBodyLBS mbReqBody
              , addRequestHeader "Authorization" ("Bearer " <> tokenBS)
              , setRequestMethod (BS.pack $ show reqMethod)
              ]
      eResp <- try $ httpLbs request
      case eResp of
        Left err -> pure $ Left (show (err :: HttpException))
        Right resp -> do
          let respStatus = getResponseStatusCode resp
              respBody = getResponseBody resp
          if respStatus <= 299 && respStatus >= 200
            then return $ Right respBody
            else return $ Left (T.unpack $ decodeUtf8 respBody)

-- | Helper function to perform url action and return FromJSON type
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

-- | Convenience function to return a url path paramter 
-- | E.g ["hello", "world"] -> /hello/world
toPath :: [String] -> String
toPath = foldl (\acc x -> acc <> "/" <> x) ""
