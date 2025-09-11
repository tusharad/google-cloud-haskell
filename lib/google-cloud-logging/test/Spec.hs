{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Data.Aeson (eitherDecode)
import qualified Data.ByteString.Lazy.Char8 as BSL
import Test.Tasty (TestTree, defaultMain, testGroup)
import Test.Tasty.HUnit (assertBool, assertEqual, testCase)

import Google.Cloud.Logging.Logs
  ( ListLogsOps (..)
  , ListLogsResp (..)
  , Resource (..)
  , defaultListLogsOps
  , googleLoggingUrl
  )

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests =
  testGroup
    "google-cloud-logging"
    [ testCase "googleLoggingUrl is v2 endpoint" $ do
        assertEqual "Endpoint" "https://logging.googleapis.com/v2" googleLoggingUrl
    , testCase "defaultListLogsOps has all fields Nothing" $ do
        let d = defaultListLogsOps
        assertEqual "resourceNames" Nothing (resourceNames d)
        assertEqual "pageSize" Nothing (pageSize d)
        assertEqual "pageToken" Nothing (pageToken d)
    , testCase "Decode ListLogsResp with nextPageToken" $ do
        let json =
              BSL.pack
                "{\n  \"logNames\": [\"projects/p/logs/a\", \"projects/p/logs/b\"],\n  \"nextPageToken\": \"tok\"\n}"
        case eitherDecode json of
          Left err -> assertBool ("Failed to decode: " <> err) False
          Right (ListLogsResp names tok) -> do
            assertEqual "names" ["projects/p/logs/a", "projects/p/logs/b"] names
            assertEqual "token" (Just "tok") tok
    , testCase "Resource Show/Eq constructors" $ do
        assertEqual "Projects" (Projects "p") (Projects "p")
        assertEqual "Show Organizations" "Organizations \"o\"" (show (Organizations "o"))
    ]
