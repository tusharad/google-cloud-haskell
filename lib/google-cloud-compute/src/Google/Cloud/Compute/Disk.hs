{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}

module Google.Cloud.Compute.Disk
  ( listDisks
  , insertDisk
  , DiskResponse (..)
  , InsertDiskOps (..)
  , InsertDiskResponse (..)
  , deleteDisk
  , createDiskSnapshot 
  , CreateSnapshotResp (..)
  , CreateSnapshotOps (..)
  ) where

import Data.Aeson
import GHC.Generics
import Google.Cloud.Common.Core
import Google.Cloud.Compute.Common

data DiskResponse = DiskResponse
  { kind :: Maybe String
  , id :: Maybe String
  , items :: Maybe [DiskItem]
  }
  deriving (Show, Generic)

instance FromJSON DiskResponse
instance ToJSON DiskResponse

data DiskItem = DiskItem
  { kind :: Maybe String
  , id :: Maybe String
  , creationTimestamp :: Maybe String
  , name :: Maybe String
  , description :: Maybe String
  , sizeGb :: Maybe String
  , zone :: Maybe String
  , status :: Maybe String -- Assuming enum as String
  , sourceSnapshot :: Maybe String
  , sourceSnapshotId :: Maybe String
  , sourceStorageObject :: Maybe String
  }
  deriving (Show, Generic, FromJSON, ToJSON)

listDisks :: String -> String -> IO (Either String DiskResponse)
listDisks project zone_ = do
  doRequestJSON
    RequestOptions
      { reqMethod = GET
      , reqUrl = googleComputeUrl
      , mbQueryParams = Nothing
      , mbReqBody = Nothing
      , mbReqHeaders = Nothing
      , mbReqPath =
          Just $
            "/"
              <> "projects/"
              <> project
              <> "/zones/"
              <> zone_
              <> "/disks"
      }

data InsertDiskOps = InsertDiskOps
  { name :: String
  , description :: Maybe String
  , sizeGb :: String
  , zone :: String
  }
  deriving (Eq, Show, Generic, ToJSON)

data InsertDiskResponse = InsertDiskResponse
  { kind :: String
  , id :: String
  , name :: String
  , zone :: String
  }
  deriving (Eq, Show, Generic, FromJSON)

insertDisk :: String -> String -> InsertDiskOps -> IO (Either String InsertDiskResponse)
insertDisk project zone_ insertDiskOps = do
  doRequestJSON
    RequestOptions
      { reqMethod = POST
      , reqUrl = googleComputeUrl
      , mbQueryParams = Nothing
      , mbReqBody = Just $ encode insertDiskOps
      , mbReqHeaders = Nothing
      , mbReqPath =
          Just $
            "/"
              <> "projects/"
              <> project
              <> "/zones/"
              <> zone_
              <> "/disks"
      }

deleteDisk :: String -> String -> String -> IO (Either String InsertDiskResponse)
deleteDisk project zone_ diskName = do
  doRequestJSON
    RequestOptions
      { reqMethod = DELETE
      , reqUrl = googleComputeUrl
      , mbQueryParams = Nothing
      , mbReqBody = Nothing
      , mbReqHeaders = Nothing
      , mbReqPath =
          Just $
            "/"
              <> "projects/"
              <> project
              <> "/zones/"
              <> zone_
              <> "/disks/"
              <> diskName
      }

data CreateSnapshotOps = CreateSnapshotOps
  { name :: String
  , description :: Maybe String
  -- more to be added
  }
  deriving (Eq, Show, Generic, ToJSON)

data CreateSnapshotResp = CreateSnapshotResp
  { kind :: String
  , id :: String
  , name :: String
  , zone :: String
  }
  deriving (Eq, Show, Generic, FromJSON)

createDiskSnapshot ::
  String -> String -> String -> CreateSnapshotOps -> IO (Either String CreateSnapshotResp)
createDiskSnapshot project zone_ diskName createSnapshotOps = do
  doRequestJSON
    RequestOptions
      { reqMethod = POST
      , reqUrl = googleComputeUrl
      , mbQueryParams = Nothing
      , mbReqBody = Just $ encode createSnapshotOps
      , mbReqHeaders = Nothing
      , mbReqPath =
          Just $
            toPath
              [ "projects"
              , project
              , "zones"
              , zone_
              , "disks"
              , diskName
              , "createSnapshot"
              ]
      }
