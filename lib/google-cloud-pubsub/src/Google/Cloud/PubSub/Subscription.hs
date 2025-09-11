{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}

{- | This module provides functions for interacting with Google Cloud Pub/Sub subscriptions.
It includes functionality for creating, deleting subscriptions, acknowledging messages,
modifying acknowledgment deadlines, and pulling messages.
-}
module Google.Cloud.PubSub.Subscription
  ( Subscription (..)
  , PullRequest (..)
  , PullResponse (..)
  , ReceivedMessage (..)
  , AcknowledgeRequest (..)
  , ModifyAckDeadlineRequest (..)
  , SubscriptionConfig (..)
  , createSubscription
  , deleteSubscription
  , acknowledgeMessages
  , modifyAckDeadline
  , pullMessages
  , googlePubSubUrl
  ) where

import Data.Aeson
import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Lazy.Char8 as BSL
import Data.Map (Map)
import GHC.Generics (Generic)
import Google.Cloud.Common.Core
import Google.Cloud.PubSub.Common

-- | Represents a Pub/Sub subscription.
data Subscription = Subscription
  { name :: String
  -- ^ The name of the subscription in the format projects/{project}/subscriptions/{subscription}.
  , topic :: String
  -- ^ The name of the topic this subscription belongs to.
  , pushConfig :: Maybe PushConfig
  -- ^ Push configuration for the subscription.
  , ackDeadlineSeconds :: Int
  -- ^ The acknowledgment deadline in seconds.
  , retainAckedMessages :: Bool
  -- ^ Whether to retain acknowledged messages.
  , messageRetentionDuration :: Maybe String
  -- ^ Message retention duration.
  , labels :: Maybe (Map String String)
  -- ^ Labels for the subscription.
  , enableMessageOrdering :: Bool
  -- ^ Whether message ordering is enabled.
  , expirationPolicy :: Maybe ExpirationPolicy
  -- ^ Expiration policy for the subscription.
  , filter :: Maybe String
  -- ^ Filter for the subscription.
  , deadLetterPolicy :: Maybe DeadLetterPolicy
  -- ^ Dead letter policy for the subscription.
  , retryPolicy :: Maybe RetryPolicy
  -- ^ Retry policy for the subscription.
  , detached :: Maybe Bool
  -- ^ Whether the subscription is detached.
  , enableExactlyOnceDelivery :: Maybe Bool
  -- ^ Whether exactly-once delivery is enabled.
  , state :: Maybe String
  -- ^ The state of the subscription.
  }
  deriving (Show, Generic, FromJSON, ToJSON)

-- | Push configuration for a subscription.
data PushConfig = PushConfig
  { pushEndpoint :: Maybe String
  -- ^ The push endpoint URL.
  , attributes :: Maybe (Map String String)
  -- ^ Attributes for the push endpoint.
  , oidcToken :: Maybe OidcToken
  -- ^ OIDC token for authentication.
  }
  deriving (Show, Generic, FromJSON, ToJSON)

-- | OIDC token for push authentication.
data OidcToken = OidcToken
  { serviceAccountEmail :: String
  -- ^ Service account email for the token.
  , audience :: Maybe String
  -- ^ Audience for the token.
  }
  deriving (Show, Generic, FromJSON, ToJSON)

-- | Expiration policy for a subscription.
newtype ExpirationPolicy = ExpirationPolicy
  { ttl :: String
  -- ^ Time-to-live for the subscription.
  }
  deriving (Show, Generic, FromJSON, ToJSON)

-- | Dead letter policy for a subscription.
data DeadLetterPolicy = DeadLetterPolicy
  { deadLetterTopic :: String
  -- ^ The dead letter topic.
  , maxDeliveryAttempts :: Maybe Int
  -- ^ Maximum delivery attempts.
  }
  deriving (Show, Generic, FromJSON, ToJSON)

-- | Retry policy for a subscription.
data RetryPolicy = RetryPolicy
  { minimumBackoff :: Maybe String
  -- ^ Minimum backoff duration.
  , maximumBackoff :: Maybe String
  -- ^ Maximum backoff duration.
  }
  deriving (Show, Generic, FromJSON, ToJSON)

-- | Configuration for creating a subscription.
data SubscriptionConfig = SubscriptionConfig
  { topic :: String
  -- ^ The topic name for the subscription.
  , pushConfig :: Maybe PushConfig
  -- ^ Push configuration.
  , ackDeadlineSeconds :: Int
  -- ^ Acknowledgment deadline in seconds.
  , retainAckedMessages :: Bool
  -- ^ Whether to retain acknowledged messages.
  , messageRetentionDuration :: Maybe String
  -- ^ Message retention duration.
  , labels :: Maybe (Map String String)
  -- ^ Labels for the subscription.
  , enableMessageOrdering :: Bool
  -- ^ Whether message ordering is enabled.
  , expirationPolicy :: Maybe ExpirationPolicy
  -- ^ Expiration policy.
  , filter :: Maybe String
  -- ^ Filter for the subscription.
  , deadLetterPolicy :: Maybe DeadLetterPolicy
  -- ^ Dead letter policy.
  , retryPolicy :: Maybe RetryPolicy
  -- ^ Retry policy.
  , enableExactlyOnceDelivery :: Maybe Bool
  -- ^ Whether exactly-once delivery is enabled.
  }
  deriving (Show, Generic, ToJSON)

-- | Request to pull messages from a subscription.
data PullRequest = PullRequest
  { maxMessages :: Int
  -- ^ Maximum number of messages to pull.
  , returnImmediately :: Bool
  -- ^ Whether to return immediately if no messages are available.
  }
  deriving (Show, Generic, ToJSON)

-- | Response from pulling messages.
newtype PullResponse = PullResponse
  { receivedMessages :: [ReceivedMessage]
  -- ^ The received messages.
  }
  deriving (Show, Generic, FromJSON)

-- | A received message from a subscription.
data ReceivedMessage = ReceivedMessage
  { ackId :: String
  -- ^ The acknowledgment ID for the message.
  , message :: PubsubMessage
  -- ^ The actual message.
  , deliveryAttempt :: Maybe Int
  -- ^ The delivery attempt number.
  }
  deriving (Show, Generic, FromJSON)

-- | Request to acknowledge messages.
newtype AcknowledgeRequest = AcknowledgeRequest
  { ackIds :: [String]
  -- ^ The acknowledgment IDs to acknowledge.
  }
  deriving (Show, Generic, ToJSON)

-- | Request to modify acknowledgment deadline.
data ModifyAckDeadlineRequest = ModifyAckDeadlineRequest
  { ackIds :: [String]
  -- ^ The acknowledgment IDs to modify.
  , ackDeadlineSeconds :: Int
  -- ^ The new acknowledgment deadline in seconds.
  }
  deriving (Show, Generic, ToJSON)

{- | Creates a new subscription in Google Cloud Pub/Sub.

@createSubscription projectId subscriptionName config@ creates a new subscription
with the specified name and configuration. It returns either an error message
or the created 'Subscription'.

Example:

>>> let config = SubscriptionConfig "projects/my-project/topics/my-topic" Nothing 600 False Nothing Nothing False Nothing Nothing Nothing Nothing Nothing
>>> createSubscription "my-project" "my-subscription" config
Right (Subscription {name = "projects/my-project/subscriptions/my-subscription", ...})
-}
createSubscription :: String -> String -> SubscriptionConfig -> IO (Either String Subscription)
createSubscription projectId subscriptionName config =
  doRequestJSON
    RequestOptions
      { reqMethod = PUT
      , reqUrl = googlePubSubUrl
      , mbReqPath = Just ("/projects/" <> projectId <> "/subscriptions/" <> subscriptionName)
      , mbReqBody = Just (encode config)
      , mbReqHeaders = Just [("Content-Type", "application/json")]
      , mbQueryParams = Nothing
      }

{- | Deletes a subscription from Google Cloud Pub/Sub.

@deleteSubscription projectId subscriptionName@ deletes the specified subscription.
It returns either an error message or a unit value indicating success.

Example:

>>> deleteSubscription "my-project" "my-subscription"
Right ()
-}
deleteSubscription :: String -> String -> IO (Either String ())
deleteSubscription projectId subscriptionName =
  doRequestJSON
    RequestOptions
      { reqMethod = DELETE
      , reqUrl = googlePubSubUrl
      , mbReqPath = Just ("/projects/" <> projectId <> "/subscriptions/" <> subscriptionName)
      , mbReqBody = Nothing
      , mbReqHeaders = Nothing
      , mbQueryParams = Nothing
      }

{- | Acknowledges messages in a subscription.

@acknowledgeMessages projectId subscriptionName ackIds@ acknowledges the messages
with the given acknowledgment IDs. It returns either an error message or a unit
value indicating success.

Example:

>>> acknowledgeMessages "my-project" "my-subscription" ["ack-id-1", "ack-id-2"]
Right ()
-}
acknowledgeMessages :: String -> String -> [String] -> IO (Either String ())
acknowledgeMessages projectId subscriptionName ackIds =
  doRequestJSON
    RequestOptions
      { reqMethod = POST
      , reqUrl = googlePubSubUrl
      , mbReqPath =
          Just ("/projects/" <> projectId <> "/subscriptions/" <> subscriptionName <> ":acknowledge")
      , mbReqBody = Just (encode $ AcknowledgeRequest ackIds)
      , mbReqHeaders = Just [("Content-Type", "application/json")]
      , mbQueryParams = Nothing
      }

{- | Modifies the acknowledgment deadline for messages.

@modifyAckDeadline projectId subscriptionName ackIds newDeadline@ modifies
the acknowledgment deadline for the given messages. It returns either an error
message or a unit value indicating success.

Example:

>>> modifyAckDeadline "my-project" "my-subscription" ["ack-id-1"] 600
Right ()
-}
modifyAckDeadline :: String -> String -> [String] -> Int -> IO (Either String ())
modifyAckDeadline projectId subscriptionName ackIds newDeadline =
  doRequestJSON
    RequestOptions
      { reqMethod = POST
      , reqUrl = googlePubSubUrl
      , mbReqPath =
          Just ("/projects/" <> projectId <> "/subscriptions/" <> subscriptionName <> ":modifyAckDeadline")
      , mbReqBody = Just (encode $ ModifyAckDeadlineRequest ackIds newDeadline)
      , mbReqHeaders = Just [("Content-Type", "application/json")]
      , mbQueryParams = Nothing
      }

{- | Pulls messages from a subscription.

@pullMessages projectId subscriptionName maxMessages@ pulls up to maxMessages
from the specified subscription. It returns either an error message or a
'PullResponse' containing the received messages.

Example:

>>> pullMessages "my-project" "my-subscription" 10
Right (PullResponse {receivedMessages = [...]})
-}
pullMessages :: String -> String -> Int -> IO (Either String PullResponse)
pullMessages projectId subscriptionName maxMessages =
  doRequestJSON
    RequestOptions
      { reqMethod = POST
      , reqUrl = googlePubSubUrl
      , mbReqPath = Just ("/projects/" <> projectId <> "/subscriptions/" <> subscriptionName <> ":pull")
      , mbReqBody = Just (encode $ PullRequest maxMessages False)
      , mbReqHeaders = Just [("Content-Type", "application/json")]
      , mbQueryParams = Nothing
      }

-- | Google Cloud Pub/Sub API base URL
googlePubSubUrl :: String
googlePubSubUrl = "https://pubsub.googleapis.com/v1"
