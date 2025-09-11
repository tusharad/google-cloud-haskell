module Main (main) where

import Compute
import Logging
import PubSub
import Storage
import System.Environment (lookupEnv)

main :: IO ()
main = do
  mbSel <- lookupEnv "EXAMPLE"
  case mbSel of
    Just "pubsub" -> PubSub.runApp
    Just "storage" -> Storage.runApp
    Just "logging" -> Logging.runApp
    Just "compute" -> Compute.runApp
    _ -> do
      putStrLn "Set EXAMPLE=pubsub|storage|logging|compute"
      PubSub.runApp
