# google-cloud-pubsub

A thin wrapper around Google Cloud Pub/Sub for Haskell.

## Features

### Topic Operations
- Create topics
- Delete topics  
- Publish messages to topics

### Subscription Operations
- Create subscriptions
- Delete subscriptions
- Acknowledge messages
- Modify acknowledgment deadlines
- Pull messages from subscriptions

## Usage

### Topic Operations

```haskell
import Google.Cloud.PubSub.Topic

-- Create a topic
result <- createTopic "my-project" "my-topic"
case result of
  Left err -> putStrLn $ "Error: " ++ err
  Right topic -> print topic

-- Publish a message
let message = Message "Hello World" Nothing Nothing
publishResult <- publishMessage "my-project" "my-topic" [message]
case publishResult of
  Left err -> putStrLn $ "Error: " ++ err
  Right response -> print $ messageIds response

-- Delete a topic
deleteResult <- deleteTopic "my-project" "my-topic"
case deleteResult of
  Left err -> putStrLn $ "Error: " ++ err
  Right _ -> putStrLn "Topic deleted successfully"
```

### Subscription Operations

```haskell
import Google.Cloud.PubSub.Subscription

-- Create a subscription
let config = SubscriptionConfig 
  { topic = "projects/my-project/topics/my-topic"
  , pushConfig = Nothing
  , ackDeadlineSeconds = 600
  , retainAckedMessages = False
  , messageRetentionDuration = Nothing
  , labels = Nothing
  , enableMessageOrdering = False
  , expirationPolicy = Nothing
  , filter = Nothing
  , deadLetterPolicy = Nothing
  , retryPolicy = Nothing
  , enableExactlyOnceDelivery = Nothing
  }

createResult <- createSubscription "my-project" "my-subscription" config
case createResult of
  Left err -> putStrLn $ "Error: " ++ err
  Right subscription -> print subscription

-- Pull messages
pullResult <- pullMessages "my-project" "my-subscription" 10
case pullResult of
  Left err -> putStrLn $ "Error: " ++ err
  Right response -> do
    mapM_ print $ receivedMessages response
    -- Acknowledge messages
    let ackIds = map ackId $ receivedMessages response
    ackResult <- acknowledgeMessages "my-project" "my-subscription" ackIds
    case ackResult of
      Left err -> putStrLn $ "Ack error: " ++ err
      Right _ -> putStrLn "Messages acknowledged"

-- Delete a subscription
deleteResult <- deleteSubscription "my-project" "my-subscription"
case deleteResult of
  Left err -> putStrLn $ "Error: " ++ err
  Right _ -> putStrLn "Subscription deleted successfully"
```

## Authentication

This library uses the same authentication mechanism as other Google Cloud libraries. Set up authentication by:

1. Setting the `GOOGLE_APPLICATION_CREDENTIALS` environment variable to point to your service account key file, or
2. Using `gcloud auth application-default login` if running on a Google Cloud environment

## Dependencies

- `google-cloud-common` - Common functionality for Google Cloud libraries
