module Storage (runApp) where

import qualified Data.ByteString.Lazy.Char8 as BSL
import Data.Maybe
import Google.Cloud.Storage.Bucket
import System.Environment (lookupEnv)

getEnvOr :: String -> String -> IO String
getEnvOr key def = do
  m <- lookupEnv key
  pure (fromMaybe def m)

runApp :: IO ()
runApp = do
  pId <- getEnvOr "PROJECT_ID" "haskread"
  bucket <- getEnvOr "BUCKET" "haskread_example-bucket-12345"
  objectName <- getEnvOr "OBJECT" "hello.txt"
  l <- getEnvOr "LOCATION" "US"

  eListBefore <- listBuckets pId
  print eListBefore

  let createData =
        CreateBucketData
          { name = bucket
          , location = Location l
          , storageClass = STANDARD
          , projectId = pId
          }
  eCreate <- createBucket createData
  print eCreate

  eUpload <- uploadObject bucket objectName (BSL.pack "hello world")
  print eUpload

  eDownload <- downloadObject bucket objectName
  print eDownload

  eDelObj <- deleteObject bucket objectName
  print eDelObj

  eDelBucket <- deleteBucket bucket
  print eDelBucket
