{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Data.Aeson (eitherDecode)
import qualified Data.ByteString.Lazy.Char8 as BSL
import Test.Tasty (TestTree, defaultMain, testGroup)
import Test.Tasty.HUnit (assertBool, assertEqual, testCase)

import Google.Cloud.Storage.Bucket
  ( Buckets (..)
  , CopyObjectResp (..)
  , Location (..)
  , StorageClass (..)
  , googleStorageUrl
  )
import qualified Google.Cloud.Storage.Bucket as CopyObject (CopyObjectResp (..))

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests =
  testGroup
    "google-cloud-storage"
    [ testCase "googleStorageUrl is JSON API buckets endpoint" $ do
        assertEqual
          "Expected JSON API base for buckets"
          "https://storage.googleapis.com/storage/v1/b"
          googleStorageUrl
    , testCase "Location Show instance renders wrapped string" $ do
        assertEqual "Show Location" "us-central1" (show (Location "us-central1"))
    , testCase "StorageClass Show values match API names" $ do
        assertEqual "STANDARD" "STANDARD" (show STANDARD)
        assertEqual "NEARLINE" "NEARLINE" (show NEARLINE)
        assertEqual "COLDLINE" "COLDLINE" (show COLDLINE)
        assertEqual "ARCHIVE" "ARCHIVE" (show ARCHIVE)
    , testCase "Decode CopyObjectResp from JSON (objects.rewrite)" $ do
        let json =
              BSL.pack
                "{\n"
                <> "  \"kind\": \"storage#rewriteResponse\",\n"
                <> "  \"totalBytesRewritten\": \"12345\",\n"
                <> "  \"objectSize\": \"12345\",\n"
                <> "  \"done\": true,\n"
                <> "  \"rewriteToken\": null\n"
                <> "}"
        case eitherDecode json of
          Left err -> assertBool ("Failed to decode: " <> err) False
          Right resp -> do
            let expected =
                  CopyObjectResp
                    { CopyObject.kind = "storage#rewriteResponse"
                    , totalBytesRewritten = "12345"
                    , objectSize = "12345"
                    , done = True
                    , rewriteToken = Nothing
                    }
            assertEqual "CopyObjectResp" expected resp
    , testCase "Decode Buckets with empty items" $ do
        let json = BSL.pack "{\n  \"kind\": \"storage#buckets\",\n  \"items\": []\n}"
        case eitherDecode json of
          Left err -> assertBool ("Failed to decode: " <> err) False
          Right (Buckets kindVal itemsVal) -> do
            assertEqual "Buckets.kind" "storage#buckets" kindVal
            assertEqual "Buckets.items length" 0 (length itemsVal)
    ]
