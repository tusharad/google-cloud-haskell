module Lib
    ( someFunc
    ) where

import Google.Cloud.Storage.Bucket
import qualified Data.ByteString.Lazy as BSL

someFunc :: IO ()
someFunc = do 
    uploadObjectExample
    downloadObjectExample

uploadObjectExample :: IO ()
uploadObjectExample = do
  content <- BSL.readFile "/home/user/kubectl"
  eRes <- uploadObject 
                "haskread_example_bucket-1" 
                "kubectl" content
  print eRes

downloadObjectExample :: IO ()
downloadObjectExample = do
  let bucketName = "haskread_example_bucket-1"
      objectName = "nix-linux.tar.xz"
  
  -- Download object
  downloadResult <- downloadObject bucketName objectName
  case downloadResult of
    Left err -> putStrLn $ "Error downloading object: " ++ err
    Right content -> do 
        BSL.writeFile objectName content
  
  putStrLn "Object operations completed"
