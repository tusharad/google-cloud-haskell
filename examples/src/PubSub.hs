module PubSub (runApp) where

import Google.Cloud.PubSub.Topic
import Google.Cloud.PubSub.Subscription

runApp :: IO ()
runApp = do 
  -- Assuming creating topic and subscription is done
  -- void $ createTopic "haskread" "some-topic"
  -- let config = SubscriptionConfig "projects/haskread/topics/some-topic" Nothing 600 False Nothing Nothing False Nothing Nothing Nothing Nothing Nothing
  -- eSubscribed <- createSubscription "haskread" "some-subscription" config
  let msg = Message "Hello World 4" Nothing Nothing
  eRes <- publishMessage "haskread" "some-topic" [msg]
  print eRes
  eRespMsg <- pullMessages "haskread" "some-subscription" 5
  print eRespMsg
  putStrLn "done"
