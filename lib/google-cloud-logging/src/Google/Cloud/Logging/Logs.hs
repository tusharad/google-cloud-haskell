{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TupleSections #-}

-- | A client module for interacting with Google Cloud Logging's log management API.
--
-- This module implements the v2 REST API for listing logs from GCP resources.
-- For API details, see: <https://cloud.google.com/logging/docs/reference/v2/rest/v2/logs/list GCP Logging API Documentation>

module Google.Cloud.Logging.Logs
  ( listLogs
  , Resource (..)
  , ListLogsResp (..)
  , googleLoggingUrl
  , defaultListLogsOps 
  , ListLogsOps (..)
  , getSettings 
  ) where

import Data.Aeson
import qualified Data.ByteString.Char8 as BS
import Google.Cloud.Common.Core
import Data.Maybe

-- | Logging url for GCP Logging service 
-- url = "https://logging.googleapis.com/v2"
googleLoggingUrl :: String
googleLoggingUrl = "https://logging.googleapis.com/v2"

-- | Represents GCP resources that can contain logs.
-- Constructors take the resource identifier as a String.
data Resource
  = Projects String       -- ^ A GCP Project resource with the given project ID
  | Organizations String  -- ^ A GCP Organization resource with the given organization ID
  | BillingAccount String -- ^ A GCP Billing Account resource with the given billing account ID
  | Folders String        -- ^ A GCP Folder resource with the given folder ID
  deriving (Eq, Show)

-- | Options for customizing the 'listLogs' request
data ListLogsOps = ListLogsOps
  { resourceNames :: Maybe [String]     -- ^ List of parent resource names to search (corresponds to 'resourceNames' query parameter)
  , pageSize :: Maybe Integer     -- ^ Maximum number of results to return (server may return fewer)
  , pageToken :: Maybe String     -- ^ Pagination token from previous 'ListLogsResp'
  }
  deriving (Eq, Show)

-- | Response from the Cloud Logging API containing log resources
data ListLogsResp = ListLogsResp
  { logNames :: [String]         -- ^ List of log resource names matching the request
  , nextPageToken :: Maybe String -- ^ Pagination token for next batch of results (if any)
  }
  deriving (Eq, Show)

instance FromJSON ListLogsResp where
  parseJSON = withObject "ListLogsResp" $ \v -> ListLogsResp 
    <$> v .: "logNames"
    <*> v .:? "nextPageToken"

-- | Internal helper to convert a Resource to a URL path component
toResource :: Resource -> [String]
toResource resource =
  case resource of
    Projects str -> ["projects", str]
    Organizations str -> ["organizations", str]
    BillingAccount str -> ["billingAccounts", str]
    Folders str -> ["folders", str]

-- | List log resource names from a GCP resource
--
-- Example:
--
-- > listLogs (Projects "my-project") Nothing
--
-- This makes a request to @v2/projects/my-project/logs@
--
-- Returns 'Either' with error message or 'ListLogsResp' containing:
-- * Matching log resource names
-- * Pagination token for subsequent requests
listLogs :: Resource           -- ^ Parent GCP resource to list logs from
         -> ListLogsOps  -- ^ parameters for pagination and filtering and other resource names
         -> IO (Either String ListLogsResp)
listLogs resource ListLogsOps{..} = do
  let qParams = concat
        [ maybeToList $ fmap (("pageSize",) . Just . BS.pack . show) pageSize
        , maybeToList $ fmap (("pageToken",) . Just . BS.pack) pageToken
        , maybe [] (map (("resourceNames",) . Just . BS.pack)) resourceNames
        ]
  doRequestJSON
    RequestOptions
      { reqMethod = GET
      , reqUrl = googleLoggingUrl
      , mbQueryParams = Just qParams
      , mbReqBody = Nothing
      , mbReqHeaders = Nothing
      , mbReqPath =
          Just $ toPath $ toResource resource <> ["logs"]
      }

-- | Helper function to pass ListLogsOps
-- | Example
-- > listLogs (Projects "my-project") (defaultListLogsOps { resourceNames = Just ["projects/my-project-id"] })
defaultListLogsOps :: ListLogsOps
defaultListLogsOps = ListLogsOps {
    resourceNames = Nothing
  , pageSize = Nothing
  , pageToken = Nothing
 }


-- | https://cloud.google.com/logging/docs/reference/v2/rest/v2/Settings

-- | Represents the configuration of the default log sink in Google Cloud Logging settings.
data DefaultSinkConfig = DefaultSinkConfig
  { destination        :: Maybe String -- ^ The export destination for the default sink (e.g., a Cloud Storage bucket).
  , filter             :: Maybe String -- ^ The filter that determines which log entries are exported.
  , outputVersionFormat :: Maybe String -- ^ The format of the log entry output (e.g., 'V2').
  } deriving (Show, Eq)

instance FromJSON DefaultSinkConfig where
  parseJSON = withObject "DefaultSinkConfig" $ \v -> DefaultSinkConfig
    <$> v .:? "destination"
    <*> v .:? "filter"
    <*> v .:? "outputVersionFormat"

-- | Represents the settings resource in Google Cloud Logging.
data Settings = Settings
  { name                    :: String -- ^ The resource name of the settings.
  , kmsKeyName              :: Maybe String -- ^ The name of the Cloud KMS key used for log encryption.
  , kmsServiceAccountId     :: Maybe String -- ^ The service account ID used for accessing the KMS key.
  , storageLocation         :: Maybe String -- ^ The storage location for log data.
  , disableDefaultSink      :: Maybe Bool -- ^ Indicates whether the default sink is disabled.
  , defaultSinkConfig       :: Maybe DefaultSinkConfig -- ^ Configuration details of the default sink.
  , loggingServiceAccountId :: Maybe String -- ^ The service account ID used by the logging service.
  } deriving (Show, Eq)

instance FromJSON Settings where
  parseJSON = withObject "Settings" $ \v -> Settings
    <$> v .:  "name"
    <*> v .:? "kmsKeyName"
    <*> v .:? "kmsServiceAccountId"
    <*> v .:? "storageLocation"
    <*> v .:? "disableDefaultSink"
    <*> v .:? "defaultSinkConfig"
    <*> v .:? "loggingServiceAccountId"

-- | Fetches the logging settings for a specified resource.
--
-- This function sends a GET request to the Google Cloud Logging API to retrieve the
-- settings associated with the provided resource.
--
-- @
-- eResult <- getSettings myResource
-- case eResult of
--   Left err -> putStrLn $ "Error: " ++ err
--   Right settings -> print settings
-- @
--
-- @since 0.1.0
getSettings :: Resource -- ^ The resource for which to retrieve logging settings.
            -> IO (Either String Settings) -- ^ Either an error message or the retrieved settings.
getSettings resource =
  doRequestJSON
    RequestOptions
      { reqMethod = GET
      , reqUrl = googleLoggingUrl
      , mbQueryParams = Nothing
      , mbReqBody = Nothing
      , mbReqHeaders = Nothing
      , mbReqPath =
          Just $ toPath $ toResource resource <> ["settings"]
      }
