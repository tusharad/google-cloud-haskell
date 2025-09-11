module PubSub (runApp) where

import Data.Maybe
import Google.Cloud.PubSub.Subscription hiding
  ( ModifyAckDeadlineRequest (..)
  , Subscription (..)
  )
import Google.Cloud.PubSub.Topic hiding
  ( Topic (..)
  )
import System.Environment (lookupEnv)
import Prelude hiding (filter)

getEnvOr :: String -> String -> IO String
getEnvOr key def = do
  m <- lookupEnv key
  pure (fromMaybe def m)

runApp :: IO ()
runApp = do
  projectId <- getEnvOr "PROJECT_ID" "haskread"
  topicName <- getEnvOr "TOPIC" "some-topic"
  subName <- getEnvOr "SUBSCRIPTION" "some-sub"

  eTopic <- createTopic projectId topicName
  print eTopic

  let cfg =
        SubscriptionConfig
          { topic = "projects/" <> projectId <> "/topics/" <> topicName
          , pushConfig = Nothing
          , ackDeadlineSeconds = 10
          , retainAckedMessages = Just False
          , messageRetentionDuration = Nothing
          , labels = Nothing
          , enableMessageOrdering = Just False
          , expirationPolicy = Nothing
          , filter = Nothing
          , deadLetterPolicy = Nothing
          , retryPolicy = Nothing
          , enableExactlyOnceDelivery = Nothing
          }
  eSub <- createSubscription projectId subName cfg
  print eSub

  let msg = Message "Hello from examples" Nothing Nothing
  ePub <- publishMessage projectId topicName [msg]
  print ePub

  ePulled <- pullMessages projectId subName 5
  print ePulled

  case ePulled of
    Right (PullResponse recvd) -> do
      let acks = map ackId recvd
      if null acks
        then pure ()
        else do
          eAck <- acknowledgeMessages projectId subName acks
          print eAck
    Left _ -> pure ()

  eDelSub <- deleteSubscription projectId subName
  print eDelSub
  eDelTopic <- deleteTopic projectId topicName
  print eDelTopic
  putStrLn "done"
