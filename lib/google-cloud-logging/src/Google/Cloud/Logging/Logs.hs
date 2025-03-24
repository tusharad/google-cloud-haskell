{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}

-- | A client module for interacting with Google Cloud Logging's log management API.
--
-- This module implements the v2 REST API for listing logs from GCP resources.
-- For API details, see: <https://cloud.google.com/logging/docs/reference/v2/rest/v2/logs/list GCP Logging API Documentation>

module Google.Cloud.Logging.Logs
  ( listLogs
  , Resource (..)
  , ListLogsResp (..)
  ) where

import Data.Aeson
import GHC.Generics
import Google.Cloud.Common.Core

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
  { resourceNames :: [String]     -- ^ List of parent resource names to search (corresponds to 'resourceNames' query parameter)
  , pageSize :: Maybe Integer     -- ^ Maximum number of results to return (server may return fewer)
  , pageToken :: Maybe String     -- ^ Pagination token from previous 'ListLogsResp'
  }
  deriving (Eq, Show)

-- | Response from the Cloud Logging API containing log resources
data ListLogsResp = ListLogsResp
  { logNames :: [String]         -- ^ List of log resource names matching the request
  , nextPageToken :: Maybe String -- ^ Pagination token for next batch of results (if any)
  }
  deriving (Eq, Show, Generic, FromJSON)

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
         -> Maybe ListLogsOps  -- ^ Optional parameters for pagination and filtering
         -> IO (Either String ListLogsResp)
listLogs resource _ =
  doRequestJSON
    RequestOptions
      { reqMethod = GET
      , reqUrl = googleLoggingUrl
      , mbQueryParams = Nothing
      , mbReqBody = Nothing
      , mbReqHeaders = Nothing
      , mbReqPath =
          Just $ toPath $ toResource resource <> ["logs"]
      }
