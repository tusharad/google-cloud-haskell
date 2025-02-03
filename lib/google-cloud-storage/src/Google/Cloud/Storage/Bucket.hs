{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

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
  ) where

import Data.Aeson
import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Lazy.Char8 as BSL
import GHC.Generics (Generic)
import Google.Cloud.Common.Core 

data Buckets = Buckets
  { kind :: String
  , items :: [Bucket]
  }
  deriving (Show, Generic, FromJSON, ToJSON)

data Bucket = Bucket
  { kind :: String
  , selfLink :: String
  , id :: String
  , name :: String
  , projectNumber :: String
  , generation :: String
  , metageneration :: String
  , location :: String
  , storageClass :: String
  , etag :: String
  , timeCreated :: String
  , updated :: String
  , softDeletePolicy :: SoftDeletePolicy
  , iamConfiguration :: IAMConfiguration
  , locationType :: String
  , satisfiesPZI :: Bool
  , hierarchicalNamespace :: Maybe HierarchicalNamespace
  }
  deriving (Show, Generic, FromJSON, ToJSON)

data SoftDeletePolicy = SoftDeletePolicy
  { retentionDurationSeconds :: String
  , effectiveTime :: String
  }
  deriving (Show, Generic, FromJSON, ToJSON)

data IAMConfiguration = IAMConfiguration
  { bucketPolicyOnly :: BucketPolicyOnly
  , uniformBucketLevelAccess :: UniformBucketLevelAccess
  , publicAccessPrevention :: String
  }
  deriving (Show, Generic, FromJSON, ToJSON)

data BucketPolicyOnly = BucketPolicyOnly
  { enabled :: Bool
  , lockedTime :: Maybe String
  }
  deriving (Show, Generic, FromJSON, ToJSON)

data UniformBucketLevelAccess = UniformBucketLevelAccess
  { enabled :: Bool
  , lockedTime :: Maybe String
  }
  deriving (Show, Generic, FromJSON, ToJSON)

newtype HierarchicalNamespace = HierarchicalNamespace
  { enabled :: Bool
  }
  deriving (Show, Generic, FromJSON, ToJSON)

googleStorageUrl :: String
googleStorageUrl = "https://storage.googleapis.com/storage/v1/b"

data CreateBucketData = CreateBucketData
  { name :: String
  , location :: Location
  , storageClass :: StorageClass
  , projectId :: String
  }
  deriving (Show)

-- List Buckets
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

data StorageClass
  = STANDARD
  | NEARLINE
  | COLDLINE
  | ARCHIVE
  deriving (Eq, Show)

newtype Location = Location String
  deriving (Eq)

instance Show Location where
  show (Location location) = location

-- Create Bucket
data CreateBucketRequest = CreateBucketRequest
  { name :: String
  , location :: String
  , storageClass :: StorageClass
  }
  deriving (Show)

instance ToJSON CreateBucketRequest where
  toJSON (CreateBucketRequest name location storageClass) =
    object ["name" .= name, "location" .= location, "storageClass" .= show storageClass]

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

-- Update Bucket Storage Class
newtype UpdateStorageClassReq = UpdateStorageClassReq StorageClass
  deriving (Eq)

instance ToJSON UpdateStorageClassReq where
  toJSON (UpdateStorageClassReq storageClass) = object ["storageClass" .= show storageClass]

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

-- Fetch Bucket
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

-- Copy Object
data CopyObjectRequest = CopyObjectRequest
  { sourceBucketName :: String
  , sourceObjectName :: String
  , destinationBucketName :: String
  , destinationObject :: String
  }
  deriving (Show, Generic)

data CopyObjectResp = CopyObjectResp
  { kind :: String
  , totalBytesRewritten :: String
  , objectSize :: String
  , done :: Bool
  , rewriteToken :: Maybe String
  }
  deriving (Show, Eq, Generic, FromJSON)

-- TODO: print progress
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

-- Delete Object
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

-- Delete Bucket
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

-- Function to download an object from Google Cloud Storage
-- TODO: Implement slicing/streaming download objects
downloadObject :: String -> String -> FilePath -> IO (Either String ())
downloadObject bucketName objectName saveLocation = do
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
      BSL.writeFile saveLocation body
      return $ Right ()

-- TODO functions
{-
 - composite download object
 - upload objects (direct, stream, slice)
 -}
