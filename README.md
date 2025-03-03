
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
To use this library, ensure you have Haskell and `stack` installed. You can include the respective packages in your `stack.yaml` or `cabal.project` file.

Example usage:
```haskell
import Google.Cloud.Compute.Instance

main :: IO ()
main = do
  instances <- listInstances
  print instances
```

## Features & Modules

### Compute Engine

#### Disk Management
- `listDisks`: Lists available disks.
- `insertDisk`: Creates a new disk.
- `deleteDisk`: Deletes an existing disk.
- `createDiskSnapshot`: Creates a snapshot of a disk.

#### Network Management
- `listNetworks`: Retrieves the list of networks.

#### Instance Management
- `listInstances`: Lists compute instances.
- `deleteInstance`: Deletes an instance.
- `startInstance`: Starts an instance.
- `stopInstance`: Stops an instance.

#### Firewall Management
- `listFirewalls`: Lists firewall rules.
- `createFirewall`: Creates a new firewall rule.

### Cloud Storage
- `listBuckets`: Lists storage buckets.
- `createBucket`: Creates a new bucket.
- `deleteBucket`: Deletes a bucket.
- `copyObject`: Copies an object between buckets.
- `deleteObject`: Deletes an object.
- `downloadObject`: Downloads an object.
- `uploadObject`: Downloads an object.

### Cloud Logging
- `listLogs`: Lists available logs.

## Roadmap & TODOs
- Add more useful functions and enhancements.
- Improve test coverage and add unit tests.
- Write comprehensive documentation.
- Release on [Hackage](https://hackage.haskell.org/).

## Contributions
Contributions are welcome! Feel free to submit PRs or open issues for discussion.

## License
This project is licensed under the MIT License.

---
This repository is a Work in Progress (WIP) and subject to continuous improvements!
