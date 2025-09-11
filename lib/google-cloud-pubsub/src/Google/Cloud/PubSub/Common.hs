{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

{- |
Module      : Google.Cloud.PubSub.Common
License     : MIT
Maintainer  : maintainers@google-cloud-haskell
Stability   : experimental

Common helpers and constants for Google Cloud Pub/Sub client modules.
-}
module Google.Cloud.PubSub.Common (googlePubSubUrl) where

import Data.Aeson
import Data.Map (Map)
import GHC.Generics (Generic)

googlePubSubUrl :: String
googlePubSubUrl = "https://pubsub.googleapis.com/v1"

-- | A message to be published.
data PubsubMessage = PubsubMessage
  { data_ :: Maybe String
  -- ^ The message data (base64 encoded).
  , attributes :: Maybe (Map String String)
  -- ^ Optional attributes for the message.
  , messageId :: Maybe String
  -- ^ The message ID (set by the server).
  , publishTime :: Maybe String
  -- ^ The publish time (set by the server).
  , orderingKey :: Maybe String
  -- ^ The ordering key for the message.
  }
  deriving (Show, Eq, Generic)

instance FromJSON PubsubMessage where
  parseJSON =
    genericParseJSON
      defaultOptions
        { fieldLabelModifier = \case
            "data_" -> "data"
            other -> other
        }

instance ToJSON PubsubMessage where
  toJSON =
    genericToJSON
      defaultOptions
        { fieldLabelModifier = \case
            "data_" -> "data"
            other -> other
        }
