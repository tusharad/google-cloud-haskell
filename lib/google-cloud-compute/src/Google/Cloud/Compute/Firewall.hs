{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}

module Google.Cloud.Compute.Firewall
  ( listFirewalls
  , FirewallList (..)
  , FirewallMeta (..)
  , AllowedFirewall (..)
  , createFirewall
  , CreateFirewallOps (..)
  , CreateFirewallResp (..)
  )
where

import Data.Aeson
import GHC.Generics
import Google.Cloud.Common.Core
import Google.Cloud.Compute.Common

data FirewallList = FirewallList
  { kind :: String
  , id :: String
  , items :: [FirewallMeta]
  }
  deriving (Eq, Show, Generic, FromJSON)

data FirewallMeta = FirewallMeta
  { kind :: String
  , id :: String
  , name :: String
  , network :: String
  }
  deriving (Eq, Show, Generic, FromJSON)

listFirewalls ::
  String -> IO (Either String FirewallList)
listFirewalls project = do
  doRequestJSON
    RequestOptions
      { reqMethod = GET
      , reqUrl = googleComputeUrl
      , mbQueryParams = Nothing
      , mbReqBody = Nothing
      , mbReqHeaders = Nothing
      , mbReqPath =
          Just $
            toPath
              [ "projects"
              , project
              , "global"
              , "firewalls"
              ]
      }

data CreateFirewallOps = CreateFirewallOps
  { name :: String
  , description :: Maybe String
  , targetTags :: [String]
  , allowed :: [AllowedFirewall]
  }
  deriving (Show, Eq, Generic, ToJSON)

data AllowedFirewall = AllowedFirewall
  { iPProtocol :: String
  , ports :: [String]
  }
  deriving (Show, Eq)

instance ToJSON AllowedFirewall where
  toJSON (AllowedFirewall iPProtocol_ ports_) =
    object ["IPProtocol" .= iPProtocol_, "ports" .= ports_]

data CreateFirewallResp = CreateFirewallResp
  { kind :: String
  , id :: String
  , name :: String
  }
  deriving (Show, Eq, Generic, FromJSON)

createFirewall :: String -> CreateFirewallOps -> IO (Either String CreateFirewallResp)
createFirewall project createFirewallOps = do
  doRequestJSON
    RequestOptions
      { reqMethod = POST
      , reqUrl = googleComputeUrl
      , mbQueryParams = Nothing
      , mbReqBody = Just $ encode createFirewallOps
      , mbReqHeaders = Nothing
      , mbReqPath =
          Just $
            toPath
              [ "projects"
              , project
              , "global"
              , "firewalls"
              ]
      }
