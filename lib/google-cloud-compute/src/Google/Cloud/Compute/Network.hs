{-|
Module      : Google.Cloud.Compute.Network
Copyright   : (c) 2025 Tushar
License     : MIT
Maintainer  : 
Stability   : experimental

This module provides types and functions for interacting with Google Cloud Platform (GCP)
Compute Engine Network. It supports common operations as listing networks,
along with detailed configuration network properties.

All functions communicate with the GCP Compute Engine API v1 and return either an error
message ('Left') or a parsed response object ('Right').

For more information on the underlying API, see:
<https://cloud.google.com/compute/docs/reference/rest/v1/networks GCP Compute Network Documentation>
-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TupleSections #-}

module Google.Cloud.Compute.Network 
  ( -- * Network Operations
    listNetworks
    -- * Data Types
  , NetworkList(..)
  , NetworkMeta(..)
  , NetworkPeering(..)
  , ListNetworksQuery(..)
    -- * Defaults
  , defaultListNetworksQuery
  ) where

import Data.Aeson
import Data.Text (Text)
import Google.Cloud.Common.Core
import Google.Cloud.Compute.Common
import Data.Maybe (catMaybes)
import qualified Data.ByteString.Char8 as BS8

-- | Response structure for listNetworks operation
data NetworkList = NetworkList
  { kind :: Text
    -- ^ Type of resource (always "compute#networkList")
  , id_ :: Text
    -- ^ Unique identifier for the resource
  , items :: [NetworkMeta]
    -- ^ List of network resources
  , nextPageToken :: Maybe Text
    -- ^ Pagination token for next page
  , selfLink :: Text
    -- ^ Server-defined URL for the resource
  } deriving (Eq, Show)

instance FromJSON NetworkList where
  parseJSON = withObject "NetworkList" $ \v -> NetworkList
    <$> v .: "kind"
    <*> v .: "id"
    <*> v .: "items"
    <*> v .:? "nextPageToken"
    <*> v .: "selfLink"

instance ToJSON NetworkList where
  toJSON NetworkList{..} = object
    [ "kind" .= kind
    , "id" .= id_
    , "items" .= items 
    , "nextPageToken" .= nextPageToken 
    , "selfLink" .= selfLink 
    ]

-- | Detailed network resource metadata
data NetworkMeta = NetworkMeta
  { kind :: Text
    -- ^ Type of resource (always "compute#network")
  , id_ :: Text
    -- ^ Unique identifier for the network
  , creationTimestamp :: Text
    -- ^ Creation timestamp in RFC3339 format
  , name :: Text
    -- ^ Name of the network
  , description :: Maybe Text
    -- ^ User-provided description
  , autoCreateSubnetworks :: Maybe Bool
    -- ^ Whether to auto-create subnetworks
  , subnetworks :: [Text]
    -- ^ List of subnetworks URLs in this network
  , iPv4Range :: Maybe Text
    -- ^ IPv4 range for legacy networks
  , iPv6Range :: Maybe Text
    -- ^ IPv6 range for legacy networks
  , gatewayIPv4 :: Maybe Text
    -- ^ Gateway IPv4 address
  , mtu :: Maybe Int
    -- ^ Maximum transmission unit
  , peerings :: Maybe [NetworkPeering]
    -- ^ List of network peerings
  , selfLink :: Text
    -- ^ Server-defined URL for the resource
  , enableUlaInternalIpv6 :: Maybe Bool
    -- ^ ULA internal IPv6 support
  , internalIpv6Range :: Maybe Text
    -- ^ Internal IPv6 range
  , networkFirewallPolicyEnforcementOrder :: Maybe Text
    -- ^ Firewall policy order
  } deriving (Eq, Show)

instance FromJSON NetworkMeta where
  parseJSON = withObject "NetworkMeta" $ \v -> NetworkMeta
    <$> v .: "kind"
    <*> v .: "id"
    <*> v .: "creationTimestamp"
    <*> v .: "name"
    <*> v .:? "description"
    <*> v .:? "autoCreateSubnetworks"
    <*> v .: "subnetworks"
    <*> v .:? "IPv4Range"  -- Match API's casing
    <*> v .:? "IPv6Range"
    <*> v .:? "gatewayIPv4"
    <*> v .:? "mtu"
    <*> (v .:? "peerings")
    <*> v .: "selfLink"
    <*> v .:? "enableUlaInternalIpv6"
    <*> v .:? "internalIpv6Range"
    <*> v .:? "networkFirewallPolicyEnforcementOrder"

instance ToJSON NetworkMeta where
  toJSON NetworkMeta{..} = object $ catMaybes
    [ Just ("kind" .= kind)
    , Just ("id" .= id_)
    , Just ("creationTimestamp" .= creationTimestamp )
    , Just ("name" .= name )
    , ("description" .=) <$> description 
    , ("autoCreateSubnetworks" .=) <$> autoCreateSubnetworks
    , Just ("subnetworks" .= subnetworks)
    , ("IPv4Range" .=) <$> iPv4Range 
    , ("IPv6Range" .=) <$> iPv6Range 
    , ("gatewayIPv4" .=) <$> gatewayIPv4 
    , ("mtu" .=) <$> mtu 
    , Just ("peerings" .= peerings )
    , Just ("selfLink" .= selfLink )
    , ("enableUlaInternalIpv6" .=) <$> enableUlaInternalIpv6 
    , ("internalIpv6Range" .=) <$> internalIpv6Range 
    , ("networkFirewallPolicyEnforcementOrder" .=) <$> networkFirewallPolicyEnforcementOrder 
    ]

-- | Network peering configuration
data NetworkPeering = NetworkPeering
  { name :: Text
    -- ^ Name of the peering
  , network :: Text
    -- ^ URL of peer network
  , state :: Text
    -- ^ Peering state (ACTIVE/INACTIVE)
  , stateDetails :: Maybe Text
    -- ^ Detailed state information
  , exchangeSubnetRoutes :: Maybe Bool
    -- ^ Subnet route exchange
  , exportCustomRoutes :: Maybe Bool
    -- ^ Custom route export
  , importCustomRoutes :: Maybe Bool
    -- ^ Custom route import
  } deriving (Eq, Show)

instance FromJSON NetworkPeering where
  parseJSON = withObject "NetworkPeering" $ \v -> NetworkPeering
    <$> v .: "name"
    <*> v .: "network"
    <*> v .: "state"
    <*> v .:? "stateDetails"
    <*> v .:? "exchangeSubnetRoutes"
    <*> v .:? "exportCustomRoutes"
    <*> v .:? "importCustomRoutes"

instance ToJSON NetworkPeering where
  toJSON NetworkPeering{..} = object $ catMaybes
    [ Just ("name" .= name)
    , Just ("network" .= network)
    , Just ("state" .= state)
    , ("stateDetails" .=) <$> stateDetails
    , ("exchangeSubnetRoutes" .=) <$> exchangeSubnetRoutes
    , ("exportCustomRoutes" .=) <$> exportCustomRoutes
    , ("importCustomRoutes" .=) <$> importCustomRoutes
    ]

-- | Query parameters for listNetworks operation
data ListNetworksQuery = ListNetworksQuery
  { filter_ :: Maybe String
    -- ^ Filter expression
  , maxResults :: Maybe Int
    -- ^ Maximum results per page
  , orderBy :: Maybe String
    -- ^ Sort order
  , pageToken :: Maybe String
    -- ^ Page token
  , returnPartialSuccess :: Maybe Bool
    -- ^ Partial success return
  } deriving (Show)

-- | Default list networks query parameters
defaultListNetworksQuery :: ListNetworksQuery
defaultListNetworksQuery = ListNetworksQuery
  { filter_ = Nothing
  , maxResults = Nothing
  , orderBy = Nothing
  , pageToken = Nothing
  , returnPartialSuccess = Nothing
  }

-- | List networks in a project
listNetworks ::
  String               -- ^ GCP Project ID
  -> Maybe ListNetworksQuery  -- ^ Query parameters
  -> IO (Either String NetworkList)
listNetworks project mbQuery = do
  let queryParams = maybe [] toQueryList mbQuery
  doRequestJSON
    RequestOptions
      { reqMethod = GET
      , reqUrl = googleComputeUrl
      , mbQueryParams = Just queryParams
      , mbReqBody = Nothing
      , mbReqHeaders = Nothing
      , mbReqPath = Just $ toPath
          [ "projects"
          , project
          , "global"
          , "networks"
          ]
      }
  where
    toQueryList :: ListNetworksQuery -> [(BS8.ByteString, Maybe BS8.ByteString)]
    toQueryList ListNetworksQuery{..} =
      catMaybes
        [ ("filter",) . Just . BS8.pack <$> filter_
        , ("maxResults",) . Just . BS8.pack . show <$> maxResults
        , ("orderBy",) . Just . BS8.pack <$> orderBy
        , ("pageToken",) . Just . BS8.pack <$> pageToken
        , ("returnPartialSuccess",) . Just . BS8.pack . show <$> returnPartialSuccess
        ]
