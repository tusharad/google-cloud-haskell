# google-cloud-compute

Haskell idiomatic client for [Google Cloud Platform](https://cloud.google.com/) Compute service.

Full docs are available at https://github.com/tusharad/google-cloud-haskell

## Installation

- Cabal: add to your `.cabal`
  - `build-depends: google-cloud-compute == 1.1.0.0`
- Stack: add to your `package.yaml`
  - `dependencies: - google-cloud-compute == 1.1.0.0`

This package depends on `google-cloud-common` for auth and HTTP helpers.

## Authentication

Authentication is handled by `google-cloud-common` and follows this order:

1. Use `GOOGLE_APPLICATION_CREDENTIALS` to load a Service Account JSON and
   exchange a signed JWT for an access token.
2. Otherwise, use the Compute metadata server (suitable for GCE/GKE).

## Examples

Below are minimal examples. See `examples/` in the repo for more.

```haskell
{-# LANGUAGE OverloadedStrings #-}

import Google.Cloud.Compute.Instance

-- List instances
listInstancesExample :: IO ()
listInstancesExample = do
  let projectId = "my-gcp-project"
      zone = "us-central1-a"
      query = defaultListInstancesQuery
  result <- listInstances projectId zone (Just query)
  case result of
    Left err -> putStrLn ("Error: " <> err)
    Right instances -> print instances

-- Create an instance
createInstanceExample :: IO ()
createInstanceExample = do
  let projectId = "my-gcp-project"
      zone = "us-central1-a"
      instanceName = "my-instance"
      machineType = "e2-medium"
      ops = defaultInsertInstanceOps projectId zone instanceName machineType
  res <- insertInstance projectId zone ops (Just (InsertInstanceQuery (Just "req-123") Nothing))
  case res of
    Left err -> putStrLn ("Create error: " <> err)
    Right _  -> putStrLn "Create initiated"

-- Start/Stop an instance
startStopExample :: IO ()
startStopExample = do
  let projectId = "my-gcp-project"
      zone = "us-central1-a"
      name = "my-instance"
      reqId = defaultRequestIdQuery "req-456"
  _ <- startInstance projectId zone name (Just reqId)
  _ <- stopInstance  projectId zone name (Just reqId)
  putStrLn "Start/Stop submitted"
```

Additional modules:

- `Google.Cloud.Compute.Disk`
- `Google.Cloud.Compute.Firewall`
- `Google.Cloud.Compute.Network`

## License

MIT © Contributors
