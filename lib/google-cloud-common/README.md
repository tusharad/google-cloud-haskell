# google-cloud-common

Shared utilities for Google Cloud Haskell clients.

This package provides common authentication and HTTP helpers used by the
`google-cloud-*` libraries (Storage, Compute, Logging, Pub/Sub, ...).

## Installation

Add the dependency to your build tool:

- Cabal: add to your `.cabal` file
  - `build-depends: google-cloud-common == 1.1.0.0`
- Stack: add to your `package.yaml`
  - `dependencies: - google-cloud-common == 1.1.0.0`

## Authentication

Two resolution paths are supported by `genAccessToken`:

1. Service Account JSON via `GOOGLE_APPLICATION_CREDENTIALS`
   - The environment variable must point to a service account key JSON file.
   - A signed JWT is created and exchanged for an OAuth2 access token.
2. Compute metadata server (GCE/GKE)
   - If the env var is not set, a token is fetched from the metadata server.
   - Useful when running on GCE/GKE with a default service account.

## Quick start

Below is a minimal example that fetches an access token and performs a JSON GET
request using the helpers from this package.

```haskell
{-# LANGUAGE OverloadedStrings #-}

import qualified Data.ByteString.Lazy.Char8 as BSL
import Google.Cloud.Common.Core

main :: IO ()
main = do
  -- Acquire an OAuth2 access token (service account JSON or metadata server)
  eToken <- genAccessToken
  case eToken of
    Left err -> putStrLn ("Auth error: " <> err)
    Right _ -> do
      let req = RequestOptions
            { reqMethod = GET
            , reqUrl = "https://www.googleapis.com"
            , mbReqHeaders = Nothing
            , mbQueryParams = Nothing
            , mbReqBody = Nothing
            , mbReqPath = Just "/oauth2/v1/certs"
            }
      eResp <- doRequest req
      case eResp of
        Left httpErr -> putStrLn ("HTTP error: " <> httpErr)
        Right body   -> BSL.putStrLn body
```

## What this package provides

- `genAccessToken`: Obtain an OAuth2 bearer token (with caching)
- `doRequest` / `doRequestJSON` / `doRequestRaw`: Thin HTTP helpers
- `RequestOptions` and `RequestMethod` types
- `toPath`: Simple URL path builder

## License

MIT © Contributors

