module Compute (runApp) where

import Data.Maybe
import Google.Cloud.Compute.Disk
import Google.Cloud.Compute.Firewall
import qualified Google.Cloud.Compute.Firewall as FireWallOps hiding
  ( FirewallMeta (..)
  )
import Google.Cloud.Compute.Instance
import Google.Cloud.Compute.Network
import System.Environment (lookupEnv)

getEnvOr :: String -> String -> IO String
getEnvOr k def = do
  m <- lookupEnv k
  pure (fromMaybe def m)

runApp :: IO ()
runApp = do
  projectId <- getEnvOr "PROJECT_ID" "haskread"
  z <- getEnvOr "ZONE" "us-central1-a"
  diskName <- getEnvOr "DISK" "example-disk"
  fwName <- getEnvOr "FIREWALL" "example-allow-http"

  eNetworks <- listNetworks projectId (Just defaultListNetworksQuery)
  print eNetworks

  eInstances <- listInstances projectId z (Just defaultListInstancesQuery)
  print eInstances

  -- Create a small standard disk
  let diskOps = defaultInsertOps diskName "10"
  eCreateDisk <- insertDisk projectId z diskOps Nothing
  print eCreateDisk

  eDisks <- listDisks projectId z Nothing
  print eDisks

  -- Create a firewall rule allowing tcp:80 from anywhere
  let fwOps =
        (defaultCreateFirewallOps fwName)
          { FireWallOps.allowed = Just [AllowedFirewall "tcp" (Just ["80"])]
          , FireWallOps.sourceRanges = Just ["0.0.0.0/0"]
          }
  eCreateFw <- createFirewall projectId fwOps Nothing
  print eCreateFw

  eFws <- listFirewalls projectId Nothing
  print eFws

  -- Cleanup resources created by this example
  eDelDisk <- deleteDisk projectId z diskName Nothing
  print eDelDisk
