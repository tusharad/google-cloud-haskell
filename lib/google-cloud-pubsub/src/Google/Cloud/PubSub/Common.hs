{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Google.Cloud.PubSub.Common (
    PubsubMessage (..)
) where

import Data.Map (Map)
import GHC.Generics (Generic)
import Data.Aeson

-- | A message to be published.
data PubsubMessage = PubsubMessage
  { data_ :: Maybe String  -- ^ The message data (base64 encoded).
  , attributes :: Maybe (Map String String)  -- ^ Optional attributes for the message.
  , messageId :: Maybe String  -- ^ The message ID (set by the server).
  , publishTime :: Maybe String  -- ^ The publish time (set by the server).
  , orderingKey :: Maybe String  -- ^ The ordering key for the message.
  }
  deriving (Show, Eq, Generic)

instance FromJSON PubsubMessage where
  parseJSON = genericParseJSON defaultOptions
    { fieldLabelModifier = \case
        "data_" -> "data"
        other   -> other
    }

instance ToJSON PubsubMessage where
  toJSON = genericToJSON defaultOptions
    { fieldLabelModifier = \case
        "data_" -> "data"
        other   -> other
    }
