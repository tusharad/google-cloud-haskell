# google-cloud-storage

Haskell idiomatic client for [Google Cloud Platform](https://cloud.google.com/) Storage service.

Full docs are available at https://github.com/tusharad/google-cloud-haskell

## Installation

- Cabal: add to your `.cabal`
  - `build-depends: google-cloud-storage == 1.1.0.0`
- Stack: add to your `package.yaml`
  - `dependencies: - google-cloud-storage == 1.1.0.0`

This package depends on `google-cloud-common` for authentication and HTTP helpers.

## Authentication

Authentication is handled by `google-cloud-common` and follows this order:

1. Use `GOOGLE_APPLICATION_CREDENTIALS` to load a Service Account JSON and
   exchange a signed JWT for an access token.
2. Otherwise, use the Compute metadata server (suitable for GCE/GKE).

## Examples

Minimal examples using `Google.Cloud.Storage.Bucket`:

```haskell
{-# LANGUAGE OverloadedStrings #-}

import qualified Data.ByteString.Lazy.Char8 as BSL
import Google.Cloud.Storage.Bucket

-- List buckets in a project
listBucketsExample :: IO ()
listBucketsExample = do
  let projectId = "my-gcp-project"
  eRes <- listBuckets projectId
  case eRes of
    Left err -> putStrLn ("Error: " <> err)
    Right buckets -> print buckets

-- Create a bucket
createBucketExample :: IO ()
createBucketExample = do
  let newBucket = CreateBucketData
        { name = "my-unique-bucket-name"
        , location = Location "us-central1"
        , storageClass = STANDARD
        , projectId = "my-gcp-project"
        }
  eRes <- createBucket newBucket
  case eRes of
    Left err -> putStrLn ("Create error: " <> err)
    Right _  -> putStrLn "Bucket created"

-- Upload and download objects
objectOpsExample :: IO ()
objectOpsExample = do
  let bucket = "my-bucket"
      object = "my-object.txt"
  _ <- uploadObject bucket object (BSL.pack "Hello, world!")
  eBody <- downloadObject bucket object
  case eBody of
    Left err -> putStrLn ("Download error: " <> err)
    Right body -> BSL.putStrLn body

-- Copy and delete an object
copyDeleteExample :: IO ()
copyDeleteExample = do
  let req = CopyObjectRequest
        { sourceBucketName = "my-bucket"
        , sourceObjectName = "my-object.txt"
        , destinationBucketName = "another-bucket"
        , destinationObject = "copied-object.txt"
        }
  _ <- copyObject req
  _ <- deleteObject "my-bucket" "my-object.txt"
  pure ()
```

## License

MIT © Contributors
