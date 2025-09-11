{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Data.Aeson (eitherDecode, encode)
import qualified Data.ByteString.Base64 as Base64
import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Lazy.Char8 as BSL
import qualified Data.Map as M
import Test.Tasty (TestTree, defaultMain, testGroup)
import Test.Tasty.HUnit (assertBool, assertEqual, testCase)

import Google.Cloud.PubSub.Common (PubsubMessage (..), googlePubSubUrl)
import Google.Cloud.PubSub.Subscription
  ( PullRequest (..)
  , PullResponse (..)
  , ReceivedMessage (..)
  )
import Google.Cloud.PubSub.Topic
  ( Message (..)
  , PublishRequest (..)
  , PublishResponse (..)
  )

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests =
  testGroup
    "google-cloud-pubsub"
    [ testCase "googlePubSubUrl is v1 endpoint" $ do
        assertEqual "Endpoint" "https://pubsub.googleapis.com/v1" googlePubSubUrl
    , testCase "PubsubMessage JSON field mapping of data_" $ do
        let msg =
              PubsubMessage
                { data_ = Just "Zm9v"
                , attributes = Nothing
                , messageId = Nothing
                , publishTime = Nothing
                , orderingKey = Nothing
                }
            json = encode msg
        assertBool "JSON must contain \"data\" key" ("\"data\"" `BS.isInfixOf` BSL.toStrict json)
        case eitherDecode json of
          Left err -> assertBool ("Failed to decode: " <> err) False
          Right (decoded :: PubsubMessage) -> assertEqual "Roundtrip" msg decoded
    , testCase "Message gets base64-encoded into PubsubMessage via PublishRequest" $ do
        let m =
              Message {content = "hello", attributes = Just (M.fromList [("k", "v")]), orderingKey = Just "ord"}
            pr =
              PublishRequest
                [ PubsubMessage
                    (Just (BS.unpack (Base64.encode (BS.pack "hello"))))
                    (Just (M.fromList [("k", "v")]))
                    Nothing
                    Nothing
                    (Just "ord")
                ]
            encoded = encode pr
        assertBool "Contains base64 of 'hello'" ("\"aGVsbG8=\"" `BS.isInfixOf` BSL.toStrict encoded)
    , testCase "Decode PublishResponse with messageIds" $ do
        let json = BSL.pack "{\n  \"messageIds\": [\"1\", \"2\"]\n}"
        case eitherDecode json of
          Left err -> assertBool ("Failed to decode: " <> err) False
          Right (PublishResponse mids) -> assertEqual "IDs" ["1", "2"] mids
    , testCase "Encode PullRequest and decode PullResponse with one message" $ do
        let req = PullRequest {maxMessages = 3, returnImmediately = False}
        assertBool "maxMessages present" ("\"maxMessages\"" `BS.isInfixOf` BSL.toStrict (encode req))
        let json =
              BSL.pack
                "{\n  \"receivedMessages\": [\n    {\n      \"ackId\": \"ack-1\",\n      \"message\": {\n        \"data\": \"aGVsbG8=\",\n        \"attributes\": {\n          \"k\": \"v\"\n        }\n      },\n      \"deliveryAttempt\": 1\n    }\n  ]\n}"
        case eitherDecode json of
          Left err -> assertBool ("Failed to decode: " <> err) False
          Right (PullResponse msgs) -> do
            assertEqual "One message" 1 (length msgs)
            let ReceivedMessage {ackId = ack, deliveryAttempt = att} = head msgs
            assertEqual "ackId" "ack-1" ack
            assertEqual "deliveryAttempt" (Just 1) att
    ]
