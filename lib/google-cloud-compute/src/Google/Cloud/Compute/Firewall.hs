{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}

{- |
Module      : Google.Cloud.Compute.Firewall
Copyright   : (c) 2025 
License     : MIT
Maintainer  : 
Stability   : experimental

This module provides a Haskell client for Google Cloud Compute Engine Firewall operations.
-}
module Google.Cloud.Compute.Firewall
  ( listFirewalls
  , FirewallList (..)
  , FirewallMeta (..)
  , AllowedFirewall (..)
  , createFirewall
  , CreateFirewallOps (..)
  , CreateFirewallResp
  , Operation (..)
  , Warning (..)
  , WarningData (..)
  , LogConfig (..)
  , DeniedFirewall (..)
  , defaultCreateFirewallOps
  ) where

import Data.Aeson
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BS8
import Data.Maybe
import Google.Cloud.Common.Core
import Google.Cloud.Compute.Common

-- | Represents a warning object in API responses
data Warning = Warning
  { code :: String
  -- ^ Warning type identifier
  , message :: String
  -- ^ Human-readable warning message
  , data_ :: Maybe [WarningData]
  -- ^ Optional metadata about the warning
  }
  deriving (Show, Eq)

-- | Key-value pair providing additional context for warnings
data WarningData = WarningData
  { key :: String
  -- ^ Metadata key describing the warning aspect
  , value :: String
  -- ^ Metadata value associated with the key
  }
  deriving (Show, Eq)

-- | Response format for firewall listing operations
data FirewallList = FirewallList
  { kind :: Maybe String
  -- ^ Type identifier ("compute#firewallList")
  , id :: Maybe String
  -- ^ Unique identifier for the resource
  , items :: Maybe [FirewallMeta]
  -- ^ List of firewall rules
  , nextPageToken :: Maybe String
  -- ^ Pagination token for next page
  , selfLink :: Maybe String
  -- ^ Server-defined URL for the resource
  , warning :: Maybe Warning
  -- ^ Any warnings about the result
  }
  deriving (Show, Eq)

-- | Represents a Google Cloud Firewall Rule
data FirewallMeta = FirewallMeta
  { kind :: Maybe String
  -- ^ Type identifier ("compute#firewall")
  , id :: Maybe String
  -- ^ Unique numeric ID
  , creationTimestamp :: Maybe String
  -- ^ ISO 8601 creation timestamp
  , name :: Maybe String
  -- ^ User-provided firewall name
  , description :: Maybe String
  -- ^ Optional rule description
  , network :: Maybe String
  -- ^ Network URL the rule applies to
  , priority :: Maybe Int
  -- ^ Rule priority (0-65535, lower=higher)
  , sourceRanges :: Maybe [String]
  -- ^ Allowed source IP ranges (CIDR)
  , sourceTags :: Maybe [String]
  -- ^ Source instance tags
  , targetTags :: Maybe [String]
  -- ^ Target instance tags
  , allowed :: Maybe [AllowedFirewall]
  -- ^ Allow rules
  , denied :: Maybe [DeniedFirewall]
  -- ^ Deny rules
  , direction :: Maybe String
  -- ^ Traffic direction (INGRESS/EGRESS)
  , logConfig :: Maybe LogConfig
  -- ^ Logging configuration
  , disabled :: Maybe Bool
  -- ^ Whether rule is disabled
  , selfLink :: Maybe String
  -- ^ Server-defined URL for the rule
  }
  deriving (Show, Eq)

-- | Allow rule specification
data AllowedFirewall = AllowedFirewall
  { iPProtocol :: String
  -- ^ Protocol name (e.g., "tcp", "udp", "icmp")
  , ports :: Maybe [String]
  -- ^ Optional port numbers/ranges (e.g., "80", "8000-8080")
  }
  deriving (Show, Eq)

-- | Deny rule specification
data DeniedFirewall = DeniedFirewall
  { iPProtocol :: String
  -- ^ Protocol name (e.g., "tcp", "udp")
  , ports :: Maybe [String]
  -- ^ Optional port numbers/ranges to block
  }
  deriving (Show, Eq)

-- | Firewall rule logging configuration
data LogConfig = LogConfig
  { enable :: Bool
  -- ^ Whether to enable logging
  , metadata :: Maybe String
  -- ^ Log metadata level (EXCLUDE_ALL_METADATA or INCLUDE_ALL_METADATA)
  }
  deriving (Show, Eq)

-- | Long-running operation resource
data Operation = Operation
  { id :: Maybe String
  -- ^ Unique operation ID
  , name :: Maybe String
  -- ^ Operation name
  , operationType :: Maybe String
  -- ^ Operation type (insert, delete, etc.)
  , targetLink :: Maybe String
  -- ^ URL of resource being modified
  , status :: Maybe String
  -- ^ Current status (DONE, RUNNING, etc.)
  , progress :: Maybe Int
  -- ^ Operation progress (0-100)
  , insertTime :: Maybe String
  -- ^ ISO 8601 creation timestamp
  , startTime :: Maybe String
  -- ^ ISO 8601 start timestamp
  , endTime :: Maybe String
  -- ^ ISO 8601 completion timestamp
  , error_ :: Maybe OperationError
  -- ^ Error details if failed
  , warnings :: Maybe [Warning]
  -- ^ Warnings during operation
  , selfLink :: Maybe String
  -- ^ Server-defined URL for the operation
  }
  deriving (Show, Eq)

-- | Error information from failed operations
data OperationError = OperationError
  { errors :: [ErrorDetail]
  -- ^ List of error details
  }
  deriving (Show, Eq)

-- | Detailed error information
data ErrorDetail = ErrorDetail
  { code :: String
  -- ^ Error type identifier
  , message :: String
  -- ^ Human-readable error message
  , location :: Maybe String
  -- ^ Location in request that triggered error
  }
  deriving (Show, Eq)

-- | Firewall creation options
data CreateFirewallOps = CreateFirewallOps
  { name :: String
  -- ^ Required firewall rule name
  , description :: Maybe String
  -- ^ Optional description
  , network :: Maybe String
  -- ^ Network URL (default: "global/networks/default")
  , priority :: Maybe Int
  -- ^ Priority (default: 1000)
  , sourceRanges :: Maybe [String]
  -- ^ Source IP ranges (CIDR format)
  , sourceTags :: Maybe [String]
  -- ^ Source instance tags
  , targetTags :: Maybe [String]
  -- ^ Target instance tags
  , allowed :: Maybe [AllowedFirewall]
  -- ^ Traffic allow rules
  , denied :: Maybe [DeniedFirewall]
  -- ^ Traffic deny rules
  , direction :: Maybe String
  -- ^ Traffic direction (default: INGRESS)
  , logConfig :: Maybe LogConfig
  -- ^ Logging configuration
  , disabled :: Maybe Bool
  -- ^ Initial disabled state (default: False)
  }
  deriving (Show, Eq)

-- | Alias for operation response from firewall creation
type CreateFirewallResp = Operation

-- JSON Instances

instance FromJSON WarningData where
  parseJSON = withObject "WarningData" $ \v ->
    WarningData
      <$> v .: "key"
      <*> v .: "value"

instance ToJSON WarningData where
  toJSON (WarningData k v) =
    object
      [ "key" .= k
      , "value" .= v
      ]

instance FromJSON Warning where
  parseJSON = withObject "Warning" $ \v ->
    Warning
      <$> v .: "code"
      <*> v .: "message"
      <*> v .:? "data"

instance ToJSON Warning where
  toJSON (Warning c m d) =
    object
      [ "code" .= c
      , "message" .= m
      , "data" .= d
      ]

instance FromJSON LogConfig where
  parseJSON = withObject "LogConfig" $ \v ->
    LogConfig
      <$> v .: "enable"
      <*> v .:? "metadata"

instance ToJSON LogConfig where
  toJSON (LogConfig e m) =
    object
      [ "enable" .= e
      , "metadata" .= m
      ]

instance FromJSON AllowedFirewall where
  parseJSON = withObject "AllowedFirewall" $ \v ->
    AllowedFirewall
      <$> v .: "IPProtocol"
      <*> v .:? "ports"

instance ToJSON AllowedFirewall where
  toJSON (AllowedFirewall i p) =
    object
      [ "IPProtocol" .= i
      , "ports" .= p
      ]

instance FromJSON DeniedFirewall where
  parseJSON = withObject "DeniedFirewall" $ \v ->
    DeniedFirewall
      <$> v .: "IPProtocol"
      <*> v .:? "ports"

instance ToJSON DeniedFirewall where
  toJSON (DeniedFirewall i p) =
    object
      [ "IPProtocol" .= i
      , "ports" .= p
      ]

instance FromJSON FirewallMeta where
  parseJSON = withObject "FirewallMeta" $ \v ->
    FirewallMeta
      <$> v .:? "kind"
      <*> v .:? "id"
      <*> v .:? "creationTimestamp"
      <*> v .:? "name"
      <*> v .:? "description"
      <*> v .:? "network"
      <*> v .:? "priority"
      <*> v .:? "sourceRanges"
      <*> v .:? "sourceTags"
      <*> v .:? "targetTags"
      <*> v .:? "allowed"
      <*> v .:? "denied"
      <*> v .:? "direction"
      <*> v .:? "logConfig"
      <*> v .:? "disabled"
      <*> v .:? "selfLink"

instance ToJSON FirewallMeta where
  toJSON
    ( FirewallMeta
        k
        i
        c
        name_
        description_
        network_
        priority_
        sourceRanges_
        sourceTags_
        targetTags_
        allowed_
        denied_
        direction_
        logConfig_
        disabled_
        selfLink_
      ) =
      object
        [ "kind" .= k
        , "id" .= i
        , "creationTimestamp" .= c
        , "name" .= name_
        , "description" .= description_
        , "network" .= network_
        , "priority" .= priority_
        , "sourceRanges" .= sourceRanges_
        , "sourceTags" .= sourceTags_
        , "targetTags" .= targetTags_
        , "allowed" .= allowed_
        , "denied" .= denied_
        , "direction" .= direction_
        , "logConfig" .= logConfig_
        , "disabled" .= disabled_
        , "selfLink" .= selfLink_
        ]

instance FromJSON FirewallList where
  parseJSON = withObject "FirewallList" $ \v ->
    FirewallList
      <$> v .:? "kind"
      <*> v .:? "id"
      <*> v .:? "items"
      <*> v .:? "nextPageToken"
      <*> v .:? "selfLink"
      <*> v .:? "warning"

instance ToJSON FirewallList where
  toJSON (FirewallList kind_ id_ items_ nextPageToken_ selfLink_ warning_) =
    object
      [ "kind" .= kind_
      , "id" .= id_
      , "items" .= items_
      , "nextPageToken" .= nextPageToken_
      , "selfLink" .= selfLink_
      , "warning" .= warning_
      ]

instance FromJSON ErrorDetail where
  parseJSON = withObject "ErrorDetail" $ \v ->
    ErrorDetail
      <$> v .: "code"
      <*> v .: "message"
      <*> v .:? "location"

instance ToJSON ErrorDetail where
  toJSON (ErrorDetail code_ message_ location_) =
    object
      [ "code" .= code_
      , "message" .= message_
      , "location" .= location_
      ]

instance FromJSON OperationError where
  parseJSON = withObject "OperationError" $ \v ->
    OperationError
      <$> v .: "errors"

instance ToJSON OperationError where
  toJSON (OperationError errors_) =
    object
      [ "errors" .= errors_
      ]

instance FromJSON Operation where
  parseJSON = withObject "Operation" $ \v ->
    Operation
      <$> v .:? "id"
      <*> v .:? "name"
      <*> v .:? "operationType"
      <*> v .:? "targetLink"
      <*> v .:? "status"
      <*> v .:? "progress"
      <*> v .:? "insertTime"
      <*> v .:? "startTime"
      <*> v .:? "endTime"
      <*> v .:? "error"
      <*> v .:? "warnings"
      <*> v .:? "selfLink"

instance ToJSON Operation where
  toJSON
    ( Operation
        id_
        name_
        operationType_
        targetLink_
        status_
        progress_
        insertTime_
        startTime_
        endTime_
        error1
        warnings_
        selfLink_
      ) =
      object
        [ "id" .= id_
        , "name" .= name_
        , "operationType" .= operationType_
        , "targetLink" .= targetLink_
        , "status" .= status_
        , "progress" .= progress_
        , "insertTime" .= insertTime_
        , "startTime" .= startTime_
        , "endTime" .= endTime_
        , "error" .= error1
        , "warnings" .= warnings_
        , "selfLink" .= selfLink_
        ]

instance ToJSON CreateFirewallOps where
  toJSON
    ( CreateFirewallOps
        name_
        description_
        network_
        priority_
        sourceRanges_
        sourceTags_
        targetTags_
        allowed_
        denied_
        direction_
        logConfig_
        disabled_
      ) =
      object
        [ "name" .= name_
        , "description" .= description_
        , "network" .= network_
        , "priority" .= priority_
        , "sourceRanges" .= sourceRanges_
        , "sourceTags" .= sourceTags_
        , "targetTags" .= targetTags_
        , "allowed" .= allowed_
        , "denied" .= denied_
        , "direction" .= direction_
        , "logConfig" .= logConfig_
        , "disabled" .= disabled_
        ]

-- | Query parameters for listFirewalls
data ListFirewallsQuery = ListFirewallsQuery
  { filter0 :: Maybe String
  , maxResults :: Maybe Int
  , pageToken :: Maybe String
  , orderBy :: Maybe String
  }
  deriving (Show, Eq)

-- | Query parameters for createFirewall
data CreateFirewallQuery = CreateFirewallQuery
  { requestId :: Maybe String
  }
  deriving (Show, Eq)

-- Helper function to convert query to list of (ByteString, Maybe ByteString)
queryToList :: (a -> [(String, String)]) -> a -> [(BS.ByteString, Maybe BS.ByteString)]
queryToList f q = map (\(k, v) -> (BS8.pack k, Just $ BS8.pack v)) (f q)

-- Functions

{- | List firewall rules in a project

Example:

> listFirewalls "my-project" (Just ListFirewallsQuery { filter0 = Just "name=default-allow-*" })
-}
listFirewalls ::
  -- | GCP Project ID
  String ->
  -- | Optional query parameters
  Maybe ListFirewallsQuery ->
  -- | List result or error
  IO (Either String FirewallList)
listFirewalls project mbQuery = do
  let queryParams = maybe [] (queryToList queryToListFunc) mbQuery
  doRequestJSON
    RequestOptions
      { reqMethod = GET
      , reqUrl = googleComputeUrl
      , mbQueryParams = Just queryParams
      , mbReqBody = Nothing
      , mbReqHeaders = Nothing
      , mbReqPath = Just $ "/projects/" <> project <> "/global/firewalls"
      }
  where
    queryToListFunc :: ListFirewallsQuery -> [(String, String)]
    queryToListFunc q =
      catMaybes
        [ ("filter",) <$> filter0 q
        , ("maxResults",) . show <$> maxResults q
        , ("pageToken",) <$> pageToken q
        , ("orderBy",) <$> orderBy q
        ]

{- | Create a new firewall rule

Example:

> createFirewall "my-project"
>   (defaultCreateFirewallOps "allow-http"
>     { allowed = Just [AllowedFirewall "tcp" (Just ["80"])]
>     , sourceRanges = Just ["0.0.0.0/0"]
>   }) Nothing
-}
createFirewall ::
  -- | GCP Project ID
  String ->
  -- | Firewall configuration
  CreateFirewallOps ->
  -- | Optional request ID
  Maybe CreateFirewallQuery ->
  -- | Operation or error
  IO (Either String CreateFirewallResp)
createFirewall project createFirewallOps mbQuery = do
  let queryParams = maybe [] (queryToList queryToListFunc) mbQuery
  doRequestJSON
    RequestOptions
      { reqMethod = POST
      , reqUrl = googleComputeUrl
      , mbQueryParams = Just queryParams
      , mbReqBody = Just $ encode createFirewallOps
      , mbReqHeaders = Nothing
      , mbReqPath = Just $ "/projects/" <> project <> "/global/firewalls"
      }
  where
    queryToListFunc :: CreateFirewallQuery -> [(String, String)]
    queryToListFunc q =
      catMaybes
        [("requestId",) <$> requestId q]

-- | Default firewall creation options
--
-- Creates minimal valid configuration with required name.
-- Additional fields can be added using record update syntax:
--
-- > defaultCreateFirewallOps "web-allow" 
-- >   { description = Just "Allow web traffic"
-- >   , allowed = Just [AllowedFirewall "tcp" (Just ["80","443"])]
-- >   }
defaultCreateFirewallOps :: String -> CreateFirewallOps
defaultCreateFirewallOps firewallName =
  CreateFirewallOps
    { name = firewallName
    , description = Nothing
    , network = Nothing
    , priority = Nothing
    , sourceRanges = Nothing
    , sourceTags = Nothing
    , targetTags = Nothing
    , allowed = Nothing
    , denied = Nothing
    , direction = Nothing
    , logConfig = Nothing
    , disabled = Nothing
    }
