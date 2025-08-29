# Google Cloud Haskell Client

Haskell idiomatic clients for [Google Cloud Platform](https://cloud.google.com/) services.

## Overview
This repository provides a lightweight and simple SDK for interacting with Google Cloud Platform (GCP) services. Unlike other libraries such as [gogool](https://github.com/gogool), this implementation aims to be minimal and straightforward, acting as a direct wrapper around GCP's REST API.

## Libraries
The following libraries are currently implemented:

- [google-cloud-storage](lib/google-cloud-storage): Provides functionality for managing Google Cloud Storage buckets and objects.
- [google-cloud-compute](lib/google-cloud-compute): Supports operations related to Compute Engine instances, disks, networks, and firewalls.
- [google-cloud-logging](lib/google-cloud-logging): Enables interaction with Google Cloud Logging for log retrieval and management.

## Installation & Usage
To use this library, ensure you have Haskell and `stack` installed. You can include the respective packages in your `package.yaml` or `.cabal` file.

## Examples

### Google Cloud Compute

```haskell
import Google.Cloud.Compute.Instance

-- List all instances in a project and zone
listInstancesExample :: IO ()
listInstancesExample = do
  let projectId = "my-gcp-project"
      zone = "us-central1-a"
      query = defaultListInstancesQuery
  result <- listInstances projectId zone (Just query)
  case result of
    Left err -> putStrLn $ "Error listing instances: " ++ err
    Right instanceList -> print instanceList

-- Create a new instance
createInstanceExample :: IO ()
createInstanceExample = do
  let projectId = "my-gcp-project"
      zone = "us-central1-a"
      instanceName = "my-instance"
      machineType = "e2-medium"
      instanceOps = defaultInsertInstanceOps projectId zone instanceName machineType
      requestId = defaultRequestIdQuery "unique-request-id-123"
  result <- insertInstance projectId zone instanceOps (Just $ InsertInstanceQuery (Just "unique-request-id-123") Nothing)
  case result of
    Left err -> putStrLn $ "Error creating instance: " ++ err
    Right response -> putStrLn "Instance creation initiated successfully"

-- Start/Stop an instance
instanceControlExample :: IO ()
instanceControlExample = do
  let projectId = "my-gcp-project"
      zone = "us-central1-a"
      instanceName = "my-instance"
      requestId = defaultRequestIdQuery "unique-request-id-456"
  
  -- Start the instance
  startResult <- startInstance projectId zone instanceName (Just requestId)
  
  -- Stop the instance
  stopResult <- stopInstance projectId zone instanceName (Just requestId)
  
  putStrLn "Instance control operations completed"
```

### Google Cloud Storage

```haskell
import Google.Cloud.Storage

-- List all buckets in a project
listBucketsExample :: IO ()
listBucketsExample = do
  let projectId = "my-gcp-project"
  result <- listBuckets projectId
  case result of
    Left err -> putStrLn $ "Error listing buckets: " ++ err
    Right buckets -> print buckets

-- Create a new bucket
createBucketExample :: IO ()
createBucketExample = do
  let bucketData = CreateBucketData
        { name = "my-unique-bucket-name"
        , location = Location "us-central1"
        , storageClass = STANDARD
        , projectId = "my-gcp-project"
        }
  result <- createBucket bucketData
  case result of
    Left err -> putStrLn $ "Error creating bucket: " ++ err
    Right _ -> putStrLn "Bucket created successfully"

-- Upload and download objects
objectOperationsExample :: IO ()
objectOperationsExample = do
  let bucketName = "my-bucket"
      objectName = "my-object.txt"
      content = "Hello, Google Cloud Storage!"
  
  -- Upload object
  uploadResult <- uploadObject bucketName objectName (BSL.pack content)
  
  -- Download object
  downloadResult <- downloadObject bucketName objectName
  case downloadResult of
    Left err -> putStrLn $ "Error downloading object: " ++ err
    Right content -> putStrLn $ "Downloaded content: " ++ show content
  
  -- Copy object to another bucket
  let copyRequest = CopyObjectRequest
        { sourceBucketName = "my-bucket"
        , sourceObjectName = "my-object.txt"
        , destinationBucketName = "my-other-bucket"
        , destinationObject = "my-copied-object.txt"
        }
  copyResult <- copyObject copyRequest
  
  -- Delete object
  deleteResult <- deleteObject bucketName objectName
  
  putStrLn "Object operations completed"
```

## Authentication
### There are two ways of authentication,

1. Application will look for `GOOGLE_APPLICATION_CREDENTIALS` environment variable and read the JSON file path.
2. If the env variable is not set, it will use [metadata server](https://cloud.google.com/docs/authentication/rest#metadata-server)

## Contributions
Contributions are welcome! Feel free to submit PRs or open issues for discussion.

## License
This project is licensed under the MIT License.

---
This repository is a Work in Progress (WIP) and subject to continuous improvements!
