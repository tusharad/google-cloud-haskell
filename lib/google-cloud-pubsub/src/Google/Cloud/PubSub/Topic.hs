{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

-- | This module provides functions for interacting with Google Cloud Pub/Sub topics.
-- It includes functionality for creating, deleting topics, and publishing messages.
module Google.Cloud.PubSub.Topic
  ( Topic (..)
  , PublishRequest (..)
  , PublishResponse (..)
  , Message (..)
  , PubsubMessage (..)
  , createTopic
  , deleteTopic
  , publishMessage
  , googlePubSubUrl
  ) where

import Data.Aeson
import qualified Data.ByteString.Base64 as Base64
import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Lazy.Char8 as BSL
import Data.Map (Map)
import GHC.Generics (Generic)
import Google.Cloud.Common.Core
import Google.Cloud.PubSub.Common

-- | Represents a Pub/Sub topic.
data Topic = Topic
  { name :: String  -- ^ The name of the topic in the format projects/{project}/topics/{topic}.
  , kmsKeyName :: Maybe String  -- ^ The KMS key name for encryption, if any.
  , messageStoragePolicy :: Maybe MessageStoragePolicy  -- ^ The message storage policy.
  , labels :: Maybe (Map String String)  -- ^ Labels for the topic.
  , messageRetentionDuration :: Maybe String  -- ^ Message retention duration.
  , state :: Maybe String  -- ^ The state of the topic.
  , satisfiesPzs :: Maybe Bool  -- ^ Whether the topic satisfies PZS requirements.
  , schemaSettings :: Maybe SchemaSettings  -- ^ Schema settings for the topic.
  }
  deriving (Show, Generic, FromJSON, ToJSON)

-- | Message storage policy for a topic.
data MessageStoragePolicy = MessageStoragePolicy
  { allowedPersistenceRegions :: [String]  -- ^ Allowed regions for message persistence.
  }
  deriving (Show, Generic, FromJSON, ToJSON)

-- | Schema settings for a topic.
data SchemaSettings = SchemaSettings
  { schema :: String  -- ^ The schema name.
  , encoding :: String  -- ^ The encoding type (JSON or BINARY).
  , firstRevisionId :: Maybe String  -- ^ First revision ID.
  , lastRevisionId :: Maybe String  -- ^ Last revision ID.
  }
  deriving (Show, Generic, FromJSON, ToJSON)

-- | Request to publish messages to a topic.
data PublishRequest = PublishRequest
  { messages :: [PubsubMessage]  -- ^ The messages to publish.
  }
  deriving (Show, Generic, ToJSON)

-- | Response from publishing messages.
data PublishResponse = PublishResponse
  { messageIds :: [String]  -- ^ The message IDs of the published messages.
  }
  deriving (Show, Generic, FromJSON)

-- | Simplified message structure for easier usage.
data Message = Message
  { content :: String  -- ^ The message content.
  , attributes :: Maybe (Map String String)  -- ^ Optional attributes for the message.
  , orderingKey :: Maybe String  -- ^ The ordering key for the message.
  }
  deriving (Show, Generic)

-- | Creates a new topic in Google Cloud Pub/Sub.
--
-- @createTopic projectId topicName@ creates a new topic with the specified name
-- in the given project. It returns either an error message or the created 'Topic'.
--
-- Example:
--
-- >>> createTopic "my-project" "my-topic"
-- Right (Topic {name = "projects/my-project/topics/my-topic", ...})
createTopic :: String -> String -> IO (Either String Topic)
createTopic projectId topicName =
  doRequestJSON
    RequestOptions
      { reqMethod = PUT
      , reqUrl = googlePubSubUrl
      , mbReqPath = Just ("/projects/" <> projectId <> "/topics/" <> topicName)
      , mbReqBody = Just (encode $ object [])
      , mbReqHeaders = Just [("Content-Type", "application/json")]
      , mbQueryParams = Nothing
      }

-- | Deletes a topic from Google Cloud Pub/Sub.
--
-- @deleteTopic projectId topicName@ deletes the specified topic.
-- It returns either an error message or a unit value indicating success.
--
-- Example:
--
-- >>> deleteTopic "my-project" "my-topic"
-- Right ()
deleteTopic :: String -> String -> IO (Either String ())
deleteTopic projectId topicName =
  doRequestJSON
    RequestOptions
      { reqMethod = DELETE
      , reqUrl = googlePubSubUrl
      , mbReqPath = Just ("/projects/" <> projectId <> "/topics/" <> topicName)
      , mbReqBody = Nothing
      , mbReqHeaders = Nothing
      , mbQueryParams = Nothing
      }

-- | Publishes messages to a topic.
--
-- @publishMessage projectId topicName messages@ publishes the given messages
-- to the specified topic. It returns either an error message or a 'PublishResponse'
-- containing the message IDs.
--
-- Example:
--
-- >>> let messages = [Message "Hello World" Nothing Nothing]
-- >>> publishMessage "my-project" "my-topic" messages
-- Right (PublishResponse {messageIds = ["1234567890"]})
publishMessage :: String -> String -> [Message] -> IO (Either String PublishResponse)
publishMessage projectId topicName messages = do
  let pubsubMessages = map messageToPubsubMessage messages
      publishRequest = PublishRequest pubsubMessages
  doRequestJSON
    RequestOptions
      { reqMethod = POST
      , reqUrl = googlePubSubUrl
      , mbReqPath = Just ("/projects/" <> projectId <> "/topics/" <> topicName <> ":publish")
      , mbReqBody = Just (encode publishRequest)
      , mbReqHeaders = Just [("Content-Type", "application/json")]
      , mbQueryParams = Nothing
      }

-- | Converts a simple Message to a PubsubMessage for API calls.
messageToPubsubMessage :: Message -> PubsubMessage
messageToPubsubMessage Message {..} =
  PubsubMessage
    { data_ = Just (BS.unpack . Base64.encode $ BS.pack content)
    , attributes = attributes
    , messageId = Nothing
    , publishTime = Nothing
    , orderingKey = orderingKey
    }

-- | Google Cloud Pub/Sub API base URL
googlePubSubUrl :: String
googlePubSubUrl = "https://pubsub.googleapis.com/v1"
