{-|
Module      : Google.Cloud.Compute.Disk
Copyright   : (c) 2025
License     : MIT
Maintainer  : 
Stability   : experimental

This module provides a Haskell client for Google Cloud Compute Engine Disk operations.
-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Google.Cloud.Compute.Disk
  ( listDisks
  , insertDisk
  , DiskResponse (..)
  , InsertDiskOps (..)
  , InsertDiskResponse
  , Operation (..)
  , deleteDisk
  , createDiskSnapshot
  , CreateSnapshotResp 
  , CreateSnapshotOps (..)
  , defaultCreateSnapshotOps
  , defaultInsertOps
  ) where

import Data.Aeson
import qualified Data.ByteString.Char8 as BS
import Data.Map (Map)
import Google.Cloud.Common.Core
import Google.Cloud.Compute.Common
import Network.HTTP.Simple

-- | Represents a warning object returned by Google Cloud API operations
data Warning = Warning
  { code :: String       -- ^ Warning type identifier
  , message :: String    -- ^ Human-readable warning message
  , data_ :: Maybe [WarningData] -- ^ Optional metadata about the warning
  }
  deriving (Show, Eq)

-- | Key-value pair providing additional context for warnings
data WarningData = WarningData
  { key :: String    -- ^ Metadata key describing the warning aspect
  , value :: String  -- ^ Metadata value associated with the key
  }
  deriving (Show, Eq)

-- | Response format for disk listing operations
data DiskResponse = DiskResponse
  { kind :: Maybe String         -- ^ Type identifier ("compute#diskList")
  , id :: Maybe String           -- ^ Unique identifier for the resource
  , items :: Maybe [DiskItem]    -- ^ List of disks in the project/zone
  , nextPageToken :: Maybe String -- ^ Pagination token for next page
  , selfLink :: Maybe String     -- ^ Server-defined URL for the resource
  , warning :: Maybe Warning     -- ^ Any warnings about the result
  }
  deriving (Show, Eq)

-- | Represents a Google Cloud Persistent Disk
data DiskItem = DiskItem
  { kind :: Maybe String              -- ^ Type identifier ("compute#disk")
  , id :: Maybe String                -- ^ Unique numeric ID
  , creationTimestamp :: Maybe String -- ^ ISO 8601 creation timestamp
  , name :: Maybe String              -- ^ User-provided disk name
  , description :: Maybe String       -- ^ Optional disk description
  , sizeGb :: Maybe String            -- ^ Disk size in gigabytes
  , zone :: Maybe String              -- ^ Zone where the disk resides
  , status :: Maybe String            -- ^ Current status (READY, CREATING, etc.)
  , sourceSnapshot :: Maybe String    -- ^ Source snapshot URL if applicable
  , sourceSnapshotId :: Maybe String  -- ^ Unique ID of source snapshot
  , sourceImage :: Maybe String       -- ^ Source image URL if applicable
  , sourceImageId :: Maybe String     -- ^ Unique ID of source image
  , type_ :: Maybe String             -- ^ Disk type (e.g., "pd-standard")
  , labels :: Maybe (Map String String) -- ^ User-defined labels
  , labelFingerprint :: Maybe String  -- ^ Fingerprint for label operations
  , licenses :: Maybe [String]        -- ^ License resource URLs
  , users :: Maybe [String]           -- ^ Instances currently using the disk
  , guestOsFeatures :: Maybe [GuestOsFeature] -- ^ OS-specific features
  , diskEncryptionKey :: Maybe EncryptionKey -- ^ Disk encryption details
  , sourceDisk :: Maybe String        -- ^ Source disk URL for regional disks
  , sourceDiskId :: Maybe String      -- ^ Unique ID of source disk
  , physicalBlockSizeBytes :: Maybe String -- ^ Physical block size in bytes
  , provisionedIops :: Maybe String   -- ^ Provisioned IOPS for SSD
  , resourcePolicies :: Maybe [String] -- ^ Resource policy URLs
  , selfLink :: Maybe String          -- ^ Server-defined URL for the disk
  }
  deriving (Show, Eq)

-- | Guest OS features enabled on the disk
data GuestOsFeature = GuestOsFeature
  { type_ :: String  -- ^ Feature type (e.g., "UEFI_COMPATIBLE")
  }
  deriving (Show, Eq)

-- | Disk encryption key information
data EncryptionKey = EncryptionKey
  { rawKey :: Maybe String        -- ^ Raw encryption key (base64)
  , rsaEncryptedKey :: Maybe String -- ^ RSA-wrapped encryption key
  , sha256 :: Maybe String        -- ^ RFC 4648 base64 SHA-256 hash
  }
  deriving (Show, Eq)

-- | Long-running operation resource
data Operation = Operation
  { id :: Maybe String          -- ^ Unique operation ID
  , name :: Maybe String        -- ^ Operation name
  , zone :: Maybe String        -- ^ Zone where operation is executed
  , operationType :: Maybe String -- ^ Operation type (insert, delete, etc.)
  , targetLink :: Maybe String  -- ^ URL of resource being modified
  , status :: Maybe String      -- ^ Current status (DONE, RUNNING, etc.)
  , progress :: Maybe Int       -- ^ Operation progress (0-100)
  , insertTime :: Maybe String  -- ^ ISO 8601 creation timestamp
  , startTime :: Maybe String   -- ^ ISO 8601 start timestamp
  , endTime :: Maybe String     -- ^ ISO 8601 completion timestamp
  , error :: Maybe OperationError -- ^ Error details if failed
  , warnings :: Maybe [Warning] -- ^ Warnings during operation
  , selfLink :: Maybe String    -- ^ Server-defined URL for the operation
  }
  deriving (Show, Eq)

-- | Error information from failed operations
data OperationError = OperationError
  { errors :: [ErrorDetail] -- ^ List of error details
  }
  deriving (Show, Eq)

-- | Detailed error information
data ErrorDetail = ErrorDetail
  { code :: String         -- ^ Error type identifier
  , message :: String      -- ^ Human-readable error message
  , location :: Maybe String -- ^ Location in request that triggered error
  }
  deriving (Show, Eq)

-- | Disk creation options
data InsertDiskOps = InsertDiskOps
  { name :: String                -- ^ Required disk name
  , sizeGb :: String              -- ^ Required size in gigabytes
  , description :: Maybe String   -- ^ Optional description
  , type_ :: Maybe String         -- ^ Disk type (default: pd-standard)
  , sourceImage :: Maybe String   -- ^ Source image URL for disk creation
  , sourceSnapshot :: Maybe String -- ^ Source snapshot URL for disk creation
  , labels :: Maybe (Map String String) -- ^ Initial labels
  , guestOsFeatures :: Maybe [GuestOsFeature] -- ^ OS features to enable
  , diskEncryptionKey :: Maybe EncryptionKey -- ^ Encryption configuration
  }
  deriving (Show, Eq)

-- | Alias for operation response from disk insertion
type InsertDiskResponse = Operation

-- | Snapshot creation options
data CreateSnapshotOps = CreateSnapshotOps
  { name :: String                -- ^ Required snapshot name
  , description :: Maybe String   -- ^ Optional description
  , labels :: Maybe (Map String String) -- ^ Labels for the snapshot
  , storageLocations :: Maybe [String] -- ^ Preferred storage locations
  , snapshotEncryptionKey :: Maybe EncryptionKey -- ^ Snapshot encryption
  }
  deriving (Show, Eq)

-- | Alias for operation response from snapshot creation
type CreateSnapshotResp = Operation

data ListDisksQuery = ListDisksQuery
  { filter_ :: Maybe String
  , maxResults :: Maybe Int
  , pageToken :: Maybe String
  , orderBy :: Maybe String
  }
  deriving (Show, Eq)

-- | Query parameters for insertDisk
data InsertDiskQuery = InsertDiskQuery
  { sourceImage :: Maybe String
  , sourceImageEncryptionKey :: Maybe EncryptionKey
  }
  deriving (Show, Eq)

-- | Query parameters for deleteDisk
data DeleteDiskQuery = DeleteDiskQuery
  { force :: Maybe Bool
  }
  deriving (Show, Eq)

-- | Query parameters for createDiskSnapshot
data CreateSnapshotQuery = CreateSnapshotQuery
  { guestFlush :: Maybe Bool
  }
  deriving (Show, Eq)

-- JSON Instances

instance FromJSON WarningData where
  parseJSON = withObject "WarningData" $ \v ->
    WarningData
      <$> v .: "key"
      <*> v .: "value"

instance ToJSON WarningData where
  toJSON (WarningData key value) =
    object
      [ "key" .= key
      , "value" .= value
      ]

instance FromJSON Warning where
  parseJSON = withObject "Warning" $ \v ->
    Warning
      <$> v .: "code"
      <*> v .: "message"
      <*> v .:? "data"

instance ToJSON Warning where
  toJSON (Warning code message data_) =
    object
      [ "code" .= code
      , "message" .= message
      , "data" .= data_
      ]

instance FromJSON GuestOsFeature where
  parseJSON = withObject "GuestOsFeature" $ \v ->
    GuestOsFeature
      <$> v .: "type"

instance ToJSON GuestOsFeature where
  toJSON (GuestOsFeature type_) =
    object
      [ "type" .= type_
      ]

instance FromJSON EncryptionKey where
  parseJSON = withObject "EncryptionKey" $ \v ->
    EncryptionKey
      <$> v .:? "rawKey"
      <*> v .:? "rsaEncryptedKey"
      <*> v .:? "sha256"

instance ToJSON EncryptionKey where
  toJSON (EncryptionKey rawKey rsaEncryptedKey sha256) =
    object
      [ "rawKey" .= rawKey
      , "rsaEncryptedKey" .= rsaEncryptedKey
      , "sha256" .= sha256
      ]

instance FromJSON DiskItem where
  parseJSON = withObject "DiskItem" $ \v ->
    DiskItem
      <$> v .:? "kind"
      <*> v .:? "id"
      <*> v .:? "creationTimestamp"
      <*> v .:? "name"
      <*> v .:? "description"
      <*> v .:? "sizeGb"
      <*> v .:? "zone"
      <*> v .:? "status"
      <*> v .:? "sourceSnapshot"
      <*> v .:? "sourceSnapshotId"
      <*> v .:? "sourceImage"
      <*> v .:? "sourceImageId"
      <*> v .:? "type"
      <*> v .:? "labels"
      <*> v .:? "labelFingerprint"
      <*> v .:? "licenses"
      <*> v .:? "users"
      <*> v .:? "guestOsFeatures"
      <*> v .:? "diskEncryptionKey"
      <*> v .:? "sourceDisk"
      <*> v .:? "sourceDiskId"
      <*> v .:? "physicalBlockSizeBytes"
      <*> v .:? "provisionedIops"
      <*> v .:? "resourcePolicies"
      <*> v .:? "selfLink"

instance ToJSON DiskItem where
  toJSON
    ( DiskItem
        kind
        id_
        creationTimestamp
        name
        description
        sizeGb
        zone
        status
        sourceSnapshot
        sourceSnapshotId
        sourceImage
        sourceImageId
        type_
        labels
        labelFingerprint
        licenses
        users
        guestOsFeatures
        diskEncryptionKey
        sourceDisk
        sourceDiskId
        physicalBlockSizeBytes
        provisionedIops
        resourcePolicies
        selfLink
      ) =
      object
        [ "kind" .= kind
        , "id" .= id_
        , "creationTimestamp" .= creationTimestamp
        , "name" .= name
        , "description" .= description
        , "sizeGb" .= sizeGb
        , "zone" .= zone
        , "status" .= status
        , "sourceSnapshot" .= sourceSnapshot
        , "sourceSnapshotId" .= sourceSnapshotId
        , "sourceImage" .= sourceImage
        , "sourceImageId" .= sourceImageId
        , "type" .= type_
        , "labels" .= labels
        , "labelFingerprint" .= labelFingerprint
        , "licenses" .= licenses
        , "users" .= users
        , "guestOsFeatures" .= guestOsFeatures
        , "diskEncryptionKey" .= diskEncryptionKey
        , "sourceDisk" .= sourceDisk
        , "sourceDiskId" .= sourceDiskId
        , "physicalBlockSizeBytes" .= physicalBlockSizeBytes
        , "provisionedIops" .= provisionedIops
        , "resourcePolicies" .= resourcePolicies
        , "selfLink" .= selfLink
        ]

instance FromJSON DiskResponse where
  parseJSON = withObject "DiskResponse" $ \v ->
    DiskResponse
      <$> v .:? "kind"
      <*> v .:? "id"
      <*> v .:? "items"
      <*> v .:? "nextPageToken"
      <*> v .:? "selfLink"
      <*> v .:? "warning"

instance ToJSON DiskResponse where
  toJSON (DiskResponse kind id_ items nextPageToken selfLink warning) =
    object
      [ "kind" .= kind
      , "id" .= id_
      , "items" .= items
      , "nextPageToken" .= nextPageToken
      , "selfLink" .= selfLink
      , "warning" .= warning
      ]

instance FromJSON ErrorDetail where
  parseJSON = withObject "ErrorDetail" $ \v ->
    ErrorDetail
      <$> v .: "code"
      <*> v .: "message"
      <*> v .:? "location"

instance ToJSON ErrorDetail where
  toJSON (ErrorDetail code message location) =
    object
      [ "code" .= code
      , "message" .= message
      , "location" .= location
      ]

instance FromJSON OperationError where
  parseJSON = withObject "OperationError" $ \v ->
    OperationError
      <$> v .: "errors"

instance ToJSON OperationError where
  toJSON (OperationError errors) =
    object
      [ "errors" .= errors
      ]

instance FromJSON Operation where
  parseJSON = withObject "Operation" $ \v ->
    Operation
      <$> v .:? "id"
      <*> v .:? "name"
      <*> v .:? "zone"
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
        name
        zone
        operationType
        targetLink
        status
        progress
        insertTime
        startTime
        endTime
        error_
        warnings
        selfLink
      ) =
      object
        [ "id" .= id_
        , "name" .= name
        , "zone" .= zone
        , "operationType" .= operationType
        , "targetLink" .= targetLink
        , "status" .= status
        , "progress" .= progress
        , "insertTime" .= insertTime
        , "startTime" .= startTime
        , "endTime" .= endTime
        , "error" .= error_
        , "warnings" .= warnings
        , "selfLink" .= selfLink
        ]

instance ToJSON InsertDiskOps where
  toJSON
    ( InsertDiskOps
        name
        sizeGb
        description
        type_
        sourceImage
        sourceSnapshot
        labels
        guestOsFeatures
        diskEncryptionKey
      ) =
      object
        [ "name" .= name
        , "sizeGb" .= sizeGb
        , "description" .= description
        , "type" .= type_
        , "sourceImage" .= sourceImage
        , "sourceSnapshot" .= sourceSnapshot
        , "labels" .= labels
        , "guestOsFeatures" .= guestOsFeatures
        , "diskEncryptionKey" .= diskEncryptionKey
        ]

instance ToJSON CreateSnapshotOps where
  toJSON (CreateSnapshotOps name description labels storageLocations snapshotEncryptionKey) =
    object
      [ "name" .= name
      , "description" .= description
      , "labels" .= labels
      , "storageLocations" .= storageLocations
      , "snapshotEncryptionKey" .= snapshotEncryptionKey
      ]

-- Functions
-- | List disks in a zone
--
-- Example:
--
-- > listDisks "my-project" "us-central1-a" Nothing
listDisks :: String               -- ^ GCP Project ID
          -> String               -- ^ Zone name
          -> Maybe ListDisksQuery -- ^ Optional query parameters
          -> IO (Either String DiskResponse) -- ^ List result or error}
listDisks project zone_ mbQuery = do
  let queryParams = maybe [] queryToList mbQuery
  doRequestJSON
    RequestOptions
      { reqMethod = GET
      , reqUrl = googleComputeUrl
      , mbQueryParams = Just queryParams
      , mbReqBody = Nothing
      , mbReqHeaders = Nothing
      , mbReqPath = Just $ "/projects/" <> project <> "/zones/" <> zone_ <> "/disks"
      }
  where
    queryToList :: ListDisksQuery -> Query
    queryToList ListDisksQuery {..} =
      [ maybe mempty (\x -> ("filter", Just $ BS.pack x)) filter_
      , maybe mempty (\x -> ("maxResults", Just . BS.pack $ show x)) maxResults
      , maybe mempty (\x -> ("pageToken", Just $ BS.pack x)) pageToken
      , maybe mempty (\x -> ("orderBy", Just $ BS.pack x)) orderBy
      ]

-- | Create a new persistent disk
--
-- Example:
--
-- > insertDisk "my-project" "us-central1-a" (defaultInsertOps "disk1" "100") Nothing
insertDisk :: String               -- ^ GCP Project ID
           -> String               -- ^ Zone name
           -> InsertDiskOps        -- ^ Disk configuration
           -> Maybe InsertDiskQuery -- ^ Optional query parameters
           -> IO (Either String InsertDiskResponse) -- ^ Operation or error}
insertDisk project zone_ insertDiskOps mbQuery = do
  let queryParams = maybe [] queryToList mbQuery
  doRequestJSON
    RequestOptions
      { reqMethod = POST
      , reqUrl = googleComputeUrl
      , mbQueryParams = Just queryParams
      , mbReqBody = Just $ encode insertDiskOps
      , mbReqHeaders = Nothing
      , mbReqPath = Just $ "/projects/" <> project <> "/zones/" <> zone_ <> "/disks"
      }
  where
    queryToList :: InsertDiskQuery -> Query
    queryToList InsertDiskQuery {sourceImage, sourceImageEncryptionKey} =
      [ maybe mempty (\x -> ("sourceImage", Just $ BS.pack x)) sourceImage
      , maybe
          mempty
          (\x -> ("sourceImageEncryptionKey", Just . BS.toStrict $ encode x))
          sourceImageEncryptionKey
      ]

-- | Deletes a disk with optional query parameters

-- | Delete a persistent disk
--
-- Example:
--
-- > deleteDisk "my-project" "us-central1-a" "old-disk" (Just DeleteDiskQuery { force = True })
deleteDisk :: String               -- ^ GCP Project ID
           -> String               -- ^ Zone name
           -> String               -- ^ Disk name to delete
           -> Maybe DeleteDiskQuery -- ^ Optional force deletion
           -> IO (Either String Operation) -- ^ Operation or error
deleteDisk project zone_ diskName mbQuery = do
  let queryParams = maybe [] queryToList mbQuery
  doRequestJSON
    RequestOptions
      { reqMethod = DELETE
      , reqUrl = googleComputeUrl
      , mbQueryParams = Just queryParams
      , mbReqBody = Nothing
      , mbReqHeaders = Nothing
      , mbReqPath = Just $ "/projects/" <> project <> "/zones/" <> zone_ <> "/disks/" <> diskName
      }
  where
    queryToList :: DeleteDiskQuery -> Query
    queryToList q =
      [ maybe mempty (\x -> ("force", Just . BS.pack $ show x)) (force q)
      ]

-- | Creates a snapshot of a disk with optional query parameters
-- | Create disk snapshot
--
-- Example:
--
-- > createDiskSnapshot "my-project" "us-central1-a" "source-disk" 
-- >   (defaultCreateSnapshotOps "daily-backup") Nothing
createDiskSnapshot :: String               -- ^ GCP Project ID
                   -> String               -- ^ Zone name
                   -> String               -- ^ Source disk name
                   -> CreateSnapshotOps    -- ^ Snapshot configuration
                   -> Maybe CreateSnapshotQuery -- ^ Optional guest flush
                   -> IO (Either String CreateSnapshotResp) -- ^ Operation or error
createDiskSnapshot project zone_ diskName createSnapshotOps mbQuery = do
  let queryParams = maybe [] queryToList mbQuery
  doRequestJSON
    RequestOptions
      { reqMethod = POST
      , reqUrl = googleComputeUrl
      , mbQueryParams = Just queryParams
      , mbReqBody = Just $ encode createSnapshotOps
      , mbReqHeaders = Nothing
      , mbReqPath =
          Just $ "/projects/" <> project <> "/zones/" <> zone_ <> "/disks/" <> diskName <> "/createSnapshot"
      }
  where
    queryToList :: CreateSnapshotQuery -> Query
    queryToList q =
      [ maybe mempty (\x -> ("guestFlush", Just . BS.pack $ show x)) (guestFlush q)
      ]

-- | Default disk creation options
--
-- Creates minimal valid configuration with required name and size.
-- Other fields can be added using record update syntax:
--
-- > defaultInsertOps "new-disk" "50" { type_ = Just "pd-ssd" }
defaultInsertOps :: String -> String -> InsertDiskOps
defaultInsertOps diskName diskSizeGb =
  InsertDiskOps
    { name = diskName
    , sizeGb = diskSizeGb
    , description = Nothing
    , type_ = Nothing
    , sourceImage = Nothing
    , sourceSnapshot = Nothing
    , labels = Nothing
    , guestOsFeatures = Nothing
    , diskEncryptionKey = Nothing
    }

-- | Default snapshot creation options
--
-- Creates minimal valid configuration with required snapshot name.
-- Additional fields can be added using record update syntax:
--
-- > defaultCreateSnapshotOps "backup" { description = Just "Daily backup" }
defaultCreateSnapshotOps :: String -> CreateSnapshotOps
defaultCreateSnapshotOps snapshotName =
  CreateSnapshotOps
    { name = snapshotName
    , description = Nothing
    , labels = Nothing
    , storageLocations = Nothing
    , snapshotEncryptionKey = Nothing
    }
