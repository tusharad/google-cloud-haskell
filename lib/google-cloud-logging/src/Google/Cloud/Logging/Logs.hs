{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}

module Google.Cloud.Logging.Logs
  ( listLogs
  , Resource (..)
  , ListLogsResp (..)
  ) where

import Data.Aeson
import GHC.Generics
import Google.Cloud.Common.Core

data Resource
  = Projects String
  | Organizations String
  | BillingAccount String
  | Folders String
  deriving (Eq, Show)

data ListLogsOps = ListLogsOps
  { resourceNames :: [String]
  , pageSize :: Maybe Integer
  , pageToken :: Maybe String
  }
  deriving (Eq, Show)

data ListLogsResp = ListLogsResp
  { logNames :: [String]
  , nextPageToken :: Maybe String
  }
  deriving (Eq, Show, Generic, FromJSON)

toResource :: Resource -> [String]
toResource resource =
  case resource of
    Projects str -> ["projects", str]
    Organizations str -> ["organizations", str]
    BillingAccount str -> ["billingAccounts", str]
    Folders str -> ["folders", str]

listLogs :: Resource -> Maybe ListLogsOps -> IO (Either String ListLogsResp)
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
