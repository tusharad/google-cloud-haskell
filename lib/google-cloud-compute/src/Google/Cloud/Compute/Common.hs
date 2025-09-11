{- |
Module      : Google.Cloud.Compute.Common
License     : MIT
Maintainer  : maintainers@google-cloud-haskell
Stability   : experimental

Common helpers and constants for Google Cloud Compute client modules.
-}
module Google.Cloud.Compute.Common (googleComputeUrl) where

googleComputeUrl :: String
googleComputeUrl = "https://compute.googleapis.com/compute/v1"
