{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

-- | This module provides functions for interacting with Google Cloud Storage buckets and objects.
-- It includes functionality for listing, creating, updating, and deleting buckets, as well as
-- copying, deleting, downloading, and uploading objects.
module Google.Cloud.Storage.Bucket
  ( Buckets (..)
  , Location (..)
  , StorageClass (..)
  , CreateBucketData (..)
  , CopyObjectRequest (..)
  , listBuckets
  , createBucket
  , updateBucketStorageClass
  , fetchBucket
  , copyObject
  , deleteObject
  , deleteBucket
  , downloadObject
  , uploadObject
  , googleStorageUrl
  ) where

import Data.Aeson
import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Lazy.Char8 as BSL
import GHC.Generics (Generic)
import Google.Cloud.Common.Core 

-- | Represents a list of buckets in a Google Cloud Storage project.
data Buckets = Buckets
  { kind :: String  -- ^ The kind of item this is. For buckets, this is always "storage#buckets".
  , items :: [Bucket]  -- ^ The list of buckets.
  }
  deriving (Show, Generic, FromJSON, ToJSON)

-- | Represents a single bucket in Google Cloud Storage.
data Bucket = Bucket
  { kind :: String  -- ^ The kind of item this is. For buckets, this is always "storage#bucket".
  , selfLink :: String  -- ^ The URL of this bucket.
  , id :: String  -- ^ The ID of the bucket.
  , name :: String  -- ^ The name of the bucket.
  , projectNumber :: String  -- ^ The project number of the project the bucket belongs to.
  , generation :: String  -- ^ The generation of this bucket.
  , metageneration :: String  -- ^ The metadata generation of this bucket.
  , location :: String  -- ^ The location of the bucket.
  , storageClass :: String  -- ^ The storage class of the bucket.
  , etag :: String  -- ^ The entity tag for this bucket.
  , timeCreated :: String  -- ^ The creation time of the bucket.
  , updated :: String  -- ^ The last modification time of the bucket.
  , softDeletePolicy :: SoftDeletePolicy  -- ^ The soft delete policy of the bucket.
  , iamConfiguration :: IAMConfiguration  -- ^ The IAM configuration of the bucket.
  , locationType :: String  -- ^ The type of location (e.g., region).
  , satisfiesPZI :: Bool  -- ^ Whether the bucket satisfies the PZI (Per-Zone Isolation) requirement.
  , hierarchicalNamespace :: Maybe HierarchicalNamespace  -- ^ The hierarchical namespace configuration.
  }
  deriving (Show, Generic, FromJSON, ToJSON)

-- | Represents the soft delete policy of a bucket.
data SoftDeletePolicy = SoftDeletePolicy
  { retentionDurationSeconds :: String  -- ^ The duration in seconds that soft-deleted objects are retained.
  , effectiveTime :: String  -- ^ The time from which the policy was enforced.
  }
  deriving (Show, Generic, FromJSON, ToJSON)

-- | Represents the IAM configuration of a bucket.
data IAMConfiguration = IAMConfiguration
  { bucketPolicyOnly :: BucketPolicyOnly  -- ^ The Bucket Policy Only configuration.
  , uniformBucketLevelAccess :: UniformBucketLevelAccess  -- ^ The Uniform Bucket-Level Access configuration.
  , publicAccessPrevention :: String  -- ^ The public access prevention status.
  }
  deriving (Show, Generic, FromJSON, ToJSON)

-- | Represents the Bucket Policy Only configuration.
data BucketPolicyOnly = BucketPolicyOnly
  { enabled :: Bool  -- ^ Whether Bucket Policy Only is enabled.
  , lockedTime :: Maybe String  -- ^ The time when the configuration was locked, if applicable.
  }
  deriving (Show, Generic, FromJSON, ToJSON)

-- | Represents the Uniform Bucket-Level Access configuration.
data UniformBucketLevelAccess = UniformBucketLevelAccess
  { enabled :: Bool  -- ^ Whether Uniform Bucket-Level Access is enabled.
  , lockedTime :: Maybe String  -- ^ The time when the configuration was locked, if applicable.
  }
  deriving (Show, Generic, FromJSON, ToJSON)

-- | Represents the hierarchical namespace configuration of a bucket.
newtype HierarchicalNamespace = HierarchicalNamespace
  { enabled :: Bool  -- ^ Whether hierarchical namespace is enabled.
  }
  deriving (Show, Generic, FromJSON, ToJSON)

-- | Represents the location of a bucket.
newtype Location = Location String
  deriving (Eq)

instance Show Location where
  show (Location location) = location

-- | Represents the storage class of a bucket or object.
data StorageClass
  = STANDARD  -- ^ Standard storage class, for frequently accessed data.
  | NEARLINE  -- ^ Nearline storage class, for infrequently accessed data.
  | COLDLINE  -- ^ Coldline storage class, for rarely accessed data.
  | ARCHIVE   -- ^ Archive storage class, for long-term data archiving.
  deriving (Eq, Show)

-- | Data required to create a new bucket.
data CreateBucketData = CreateBucketData
  { name :: String  -- ^ The name of the bucket to create.
  , location :: Location  -- ^ The location where the bucket will be created.
  , storageClass :: StorageClass  -- ^ The storage class for the bucket.
  , projectId :: String  -- ^ The ID of the project in which to create the bucket.
  }
  deriving (Show)

-- | Request data for copying an object.
data CopyObjectRequest = CopyObjectRequest
  { sourceBucketName :: String  -- ^ The name of the source bucket.
  , sourceObjectName :: String  -- ^ The name of the source object.
  , destinationBucketName :: String  -- ^ The name of the destination bucket.
  , destinationObject :: String  -- ^ The name of the destination object.
  }
  deriving (Show, Generic)

-- | Response data for copying an object.
data CopyObjectResp = CopyObjectResp
  { kind :: String  -- ^ The kind of item this is.
  , totalBytesRewritten :: String  -- ^ The total number of bytes rewritten.
  , objectSize :: String  -- ^ The size of the object.
  , done :: Bool  -- ^ Whether the copy operation is complete.
  , rewriteToken :: Maybe String  -- ^ Token for continuing the rewrite operation, if not done.
  }
  deriving (Show, Eq, Generic, FromJSON)

-- | Lists all buckets in a given project.
--
-- @listBuckets projectId@ makes a request to list all buckets in the specified @projectId@.
-- It returns either an error message or a 'Buckets' object containing the list of buckets.
listBuckets :: String -> IO (Either String Buckets)
listBuckets projectId =
  doRequestJSON
    RequestOptions
      { reqMethod = GET
      , reqUrl = googleStorageUrl
      , mbQueryParams = Just [("project", Just (BS.pack projectId))]
      , mbReqBody = Nothing
      , mbReqHeaders = Nothing
      , mbReqPath = Nothing
      }

-- | Creates a new bucket in Google Cloud Storage.
--
-- @createBucket createBucketData@ creates a new bucket with the specified name, location,
-- storage class, and project ID. It returns either an error message or a unit value indicating success.
createBucket :: CreateBucketData -> IO (Either String ())
createBucket CreateBucketData {..} =
  doRequestJSON
    RequestOptions
      { reqMethod = POST
      , reqUrl = googleStorageUrl
      , mbReqPath = Nothing
      , mbReqBody = Just (encode $ CreateBucketRequest name (show location) storageClass)
      , mbReqHeaders = Just [("Content-Type", "application/json")]
      , mbQueryParams = Just [("project", Just (BS.pack projectId))]
      }

-- | Updates the storage class of an existing bucket.
--
-- @updateBucketStorageClass bucketName newStorageClass@ updates the storage class of the
-- specified bucket to @newStorageClass@. It returns either an error message or a unit value
-- indicating success.
-- -- Example:
--
-- >>> updateBucketStorageClass "my-bucket" COLDLINE
-- Right ()
updateBucketStorageClass :: String -> StorageClass -> IO (Either String ())
updateBucketStorageClass bucketName newStorageClass =
  doRequestJSON
    RequestOptions
      { reqMethod = PATCH
      , reqUrl = googleStorageUrl
      , mbReqPath = Just ("/" <> bucketName)
      , mbReqBody = Just (encode $ UpdateStorageClassReq newStorageClass)
      , mbReqHeaders = Just [("Content-Type", "application/json")]
      , mbQueryParams = Just [("fields", Just "storageClass")]
      }

-- | Fetches the metadata of a specific bucket.
--
-- @fetchBucket bucketName@ retrieves the metadata for the bucket with the given name.
-- It returns either an error message or the 'Bucket' object.
-- Example:
--
-- >>> fetchBucket "my-bucket"
-- Right (Bucket {name = "my-bucket", ...})
fetchBucket :: String -> IO (Either String Bucket)
fetchBucket bucketName =
  doRequestJSON
    RequestOptions
      { reqMethod = GET
      , reqUrl = googleStorageUrl
      , mbReqPath = Just ("/" <> bucketName)
      , mbReqBody = Nothing
      , mbReqHeaders = Nothing
      , mbQueryParams = Nothing
      }

-- | Copies an object from one bucket to another.
--
-- @copyObject copyObjectRequest@ copies the specified source object to the destination bucket
-- and object name. For large objects, this function handles the copy in multiple requests
-- using rewrite tokens. It returns either an error message or a 'CopyObjectResp' indicating
-- the status of the copy operation.
-- Example:
--
-- >>> let request = CopyObjectRequest "source-bucket" "source-object" "destination-bucket" "destination-object"
-- >>> copyObject request
-- Right (CopyObjectResp {kind = "storage#object", totalBytesRewritten = "12345", objectSize = "12345", done = True, rewriteToken = Nothing})
copyObject :: CopyObjectRequest -> IO (Either String CopyObjectResp)
copyObject CopyObjectRequest {..} = do
  go Nothing
  where
    go mbRewriteToken = do
      eRes <-
        doRequestJSON
          RequestOptions
            { reqMethod = POST
            , reqUrl = googleStorageUrl
            , mbReqPath =
                Just
                  ( "/"
                      <> sourceBucketName
                      <> "/o/"
                      <> sourceObjectName
                      <> "/rewriteTo/b/"
                      <> destinationBucketName
                      <> "/o/"
                      <> destinationObject
                  )
            , mbReqBody = case mbRewriteToken of
                Nothing -> Nothing
                (Just r) -> Just (encode $ object ["rewriteToken" .= r])
            , mbReqHeaders = Just [("Content-Length", "0")]
            , mbQueryParams = Nothing
            }
      case eRes of
        Left err -> pure $ Left (show err)
        Right resp -> do
          if done resp
            then
              return $ Right resp
            else do
              go (Just (rewriteToken resp))

-- | Deletes an object from a bucket.
--
-- @deleteObject bucketName objectName@ deletes the specified object from the given bucket.
-- It returns either an error message or a unit value indicating success.
-- Example:
--
-- >>> deleteObject "my-bucket" "my-object"
-- Right ()
deleteObject :: String -> String -> IO (Either String ())
deleteObject bucketName objectName =
  doRequestJSON
    RequestOptions
      { reqMethod = DELETE
      , reqUrl = googleStorageUrl
      , mbReqPath = Just ("/" <> bucketName <> "/o/" <> objectName)
      , mbReqBody = Nothing
      , mbReqHeaders = Nothing
      , mbQueryParams = Nothing
      }

-- | Deletes a bucket.
--
-- @deleteBucket bucketName@ deletes the specified bucket. Note that the bucket must be empty
-- before it can be deleted. It returns either an error message or a unit value indicating success.
--
-- Example:
--
-- >>> deleteBucket "my-bucket"
-- Right ()
deleteBucket :: String -> IO (Either String ())
deleteBucket bucketName =
  doRequestJSON
    RequestOptions
      { reqMethod = DELETE
      , reqUrl = googleStorageUrl
      , mbReqPath = Just ("/" <> bucketName)
      , mbReqBody = Nothing
      , mbReqHeaders = Nothing
      , mbQueryParams = Nothing
      }

-- | Downloads an object from a bucket to a local file.
--
-- @downloadObject bucketName objectName saveLocation@ returns the specified object in
-- lazy bytestring format. It returns either an error message or content indicating success.
-- Note: This function downloads the entire object at once; for large objects, consider
-- implementing streaming or slicing (planned for future versions).
-- Example:
--
-- >>> downloadObject "my-bucket" "my-object"
-- Right (bytestring content)
downloadObject :: String -> String -> IO (Either String BSL.ByteString)
downloadObject bucketName objectName = do
  let reqOpts =
        RequestOptions
          { reqMethod = GET
          , reqUrl = googleStorageUrl
          , mbQueryParams = Just [("alt", Just "media")]
          , mbReqBody = Nothing
          , mbReqHeaders = Nothing
          , mbReqPath = Just $ "/" <> bucketName <> "/o/" <> objectName
          }
  res <- doRequest reqOpts
  case res of
    Left err -> return (Left err)
    Right resp -> do
      let body = resp
      return $ Right body

-- | Uploads an object to a bucket.
--
-- @uploadObject bucketName objectName objectContent@ uploads the given content to the specified
-- object in the bucket. It returns either an error message or a unit value indicating success.
-- Note: This function uploads the entire object at once; for large objects, consider
-- implementing streaming or slicing (planned for future versions).
-- Example:
--
-- >>> uploadObject "my-bucket" "my-object" (BSL.pack "object content")
-- Right ()
uploadObject :: String -> String -> BSL.ByteString -> IO (Either String ())
uploadObject bucketName objectName objectContent = do
  doRequestJSON
    RequestOptions
      { reqMethod = POST
      , reqUrl = "https://storage.googleapis.com/upload/storage/v1/b"
      , mbReqPath = Just $ "/" <> bucketName <> "/o"
      , mbReqBody = Just objectContent 
      , mbReqHeaders = Nothing
      , mbQueryParams = Just [("uploadType", Just "media"),("name", Just $ BS.pack objectName)]
      }

-- Helper types and functions (not exported, so no Haddock documentation needed)

data CreateBucketRequest = CreateBucketRequest
  { name :: String -- ^ Name of the bucket 
  , location :: String  -- ^ Location of bucket e.g us-central1-a
  , storageClass :: StorageClass -- ^ Storage Class
  }
  deriving (Show)

instance ToJSON CreateBucketRequest where
  toJSON (CreateBucketRequest name location storageClass) =
    object ["name" .= name, "location" .= location, "storageClass" .= show storageClass]

newtype UpdateStorageClassReq = UpdateStorageClassReq StorageClass
  deriving (Eq)

instance ToJSON UpdateStorageClassReq where
  toJSON (UpdateStorageClassReq storageClass) = object ["storageClass" .= show storageClass]

-- | GCP Storage Url
googleStorageUrl :: String
googleStorageUrl = "https://storage.googleapis.com/storage/v1/b"
