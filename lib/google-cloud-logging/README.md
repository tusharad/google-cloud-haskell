# google-cloud-logging

Haskell idiomatic client for [Google Cloud Platform](https://cloud.google.com/) Logging service.

Full docs are available at https://github.com/tusharad/google-cloud-haskell

## Installation

- Cabal: add to your `.cabal`
  - `build-depends: google-cloud-logging == 1.1.0.0`
- Stack: add to your `package.yaml`
  - `dependencies: - google-cloud-logging == 1.1.0.0`

This package depends on `google-cloud-common` for auth and HTTP helpers.

## Authentication

Authentication is handled by `google-cloud-common` and follows this order:

1. Use `GOOGLE_APPLICATION_CREDENTIALS` to load a Service Account JSON and
   exchange a signed JWT for an access token.
2. Otherwise, use the Compute metadata server (suitable for GCE/GKE).

## Examples

Minimal examples using `Google.Cloud.Logging.Logs`:

```haskell
{-# LANGUAGE OverloadedStrings #-}

import Google.Cloud.Logging.Logs

-- List log names under a project
listLogsExample :: IO ()
listLogsExample = do
  let resource = Projects "my-gcp-project"
  eResp <- listLogs resource defaultListLogsOps
  case eResp of
    Left err -> putStrLn ("Error: " <> err)
    Right res -> print (logNames res)

-- Fetch logging settings for a project
getSettingsExample :: IO ()
getSettingsExample = do
  let resource = Projects "my-gcp-project"
  eRes <- getSettings resource
  case eRes of
    Left err -> putStrLn ("Settings error: " <> err)
    Right settings -> print settings
```

## License

MIT © Contributors
