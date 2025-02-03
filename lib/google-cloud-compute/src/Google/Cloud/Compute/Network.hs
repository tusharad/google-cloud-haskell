{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}

module Google.Cloud.Compute.Network 
 (listNetworks
 , NetworkList (..)
 , NetworkMeta (..)
    )
 where

import Data.Aeson
import GHC.Generics
import Google.Cloud.Common.Core
import Google.Cloud.Compute.Common

data NetworkList = NetworkList {
    kind :: String
  , id :: String
  , items :: [NetworkMeta]
 } deriving (Eq, Show, Generic, FromJSON)

data NetworkMeta = NetworkMeta {
    kind :: String
  , id :: String
  , name :: String
  , subnetworks :: [String]
 } deriving (Eq, Show, Generic, FromJSON)

listNetworks ::
  String -> IO (Either String NetworkList)
listNetworks project = do
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
              , "networks"
              ]
      }
