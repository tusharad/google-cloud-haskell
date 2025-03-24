{-|
Module      : Google.Cloud.Compute.Instance
Copyright   : (c) 2025 Tushar
License     : MIT
Maintainer  : 
Stability   : experimental

This module provides types and functions for interacting with Google Cloud Platform (GCP)
Compute Engine instances. It supports common operations such as listing, creating, deleting,
starting, and stopping instances, along with detailed configuration of instance properties.

All functions communicate with the GCP Compute Engine API v1 and return either an error
message ('Left') or a parsed response object ('Right').

For more information on the underlying API, see:
<https://cloud.google.com/compute/docs/reference/rest/v1/instances GCP Compute Instances Documentation>
-}

{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TupleSections #-}

module Google.Cloud.Compute.Instance
  ( -- Data Types
    InstanceMetadata (..)
  , InstanceDeleteResp (..)
  , InstanceStartResponse (..)
  , RunDuration (..)
  , OnInstanceTerminationAction (..)
  , Scheduling (..)
  , InstanceTerminationAction (..)
  , NodeAffinity (..)
  , Warning (..)
  , GuestAccelerator (..)
  , ReservationAffinity (..)
  , DisplayDevice (..)
  , ShieldedInstanceConfig (..)
  , ShieldedInstanceIntegrityPolicy (..)
  , UpdateAutoLearnPolicy (..)
  , ConfidentialInstanceConfig (..)
  , InstanceEncryptionKey (..)
  , NetworkPerformanceConfig (..)
  , ResourceStatus (..)
  , InstanceList (..)
  , Error_ (..)
  , ErrorDetail (..)
  , DetailedError (..)
  , ErrorInfo (..)
  , QuotaInfo (..)
  , Help (..)
  , Link (..)
  , LocalizedMessage (..)
  -- Query Parameter Types
  , ListInstancesQuery (..)
  , RequestIdQuery (..)
  , InsertInstanceOps (..)
  -- Functions
  , listInstances
  , deleteInstance
  , startInstance
  , stopInstance
  , insertInstance
  -- Default Query Constructors
  , defaultListInstancesQuery
  , defaultInsertInstanceOps
  , defaultRequestIdQuery
  )
where

import Data.Aeson
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BS8
import Data.Maybe (catMaybes)
import Data.Text (Text)
import Google.Cloud.Common.Core
import Google.Cloud.Compute.Common
import qualified Data.Map.Strict as Map

-- ** Query Parameter Types**

-- | Query parameters for listing instances
data ListInstancesQuery = ListInstancesQuery
  { filter_ :: Maybe String
    -- ^ Filter expression for filtering listed instances
  , maxResults :: Maybe Int
    -- ^ Maximum number of results per page
  , orderBy :: Maybe String
    -- ^ Sort order for results
  , pageToken :: Maybe String
    -- ^ Page token for paginated results
  , returnPartialSuccess :: Maybe Bool
    -- ^ Whether to return partial success results
  }
  deriving (Show, Eq)

-- Query parameters for operations with requestId (start, stop, delete)
data RequestIdQuery = RequestIdQuery
  { requestId :: Maybe String
  }
  deriving (Show, Eq)

-- ** Data Types with Custom JSON Instances**

data RunDuration
  = TerminationTime String
  deriving (Show, Eq)

instance FromJSON RunDuration where
  parseJSON = withObject "RunDuration" $ \o -> TerminationTime <$> o .: "terminationTime"

instance ToJSON RunDuration where
  toJSON (TerminationTime t) = object ["terminationTime" .= t]

data OnInstanceTerminationAction = DiscardLocalSsd {discardLocalSsd :: Bool}
  deriving (Show, Eq)

instance FromJSON OnInstanceTerminationAction where
  parseJSON = withObject "OnInstanceTerminationAction" $ \o -> DiscardLocalSsd <$> o .: "discardLocalSsd"

instance ToJSON OnInstanceTerminationAction where
  toJSON (DiscardLocalSsd b) = object ["discardLocalSsd" .= b]

data Scheduling = Scheduling
  { onHostMaintenance :: String
  , automaticRestart :: Bool
  , preemptible :: Bool
  , nodeAffinities :: [NodeAffinity]
  , minNodeCpus :: Int
  , locationHint :: String
  , availabilityDomain :: Int
  , provisioningModel :: String
  , instanceTerminationAction :: InstanceTerminationAction
  }
  deriving (Show, Eq)

instance FromJSON Scheduling where
  parseJSON = withObject "Scheduling" $ \o ->
    Scheduling
      <$> o .: "onHostMaintenance"
      <*> o .: "automaticRestart"
      <*> o .: "preemptible"
      <*> o .: "nodeAffinities"
      <*> o .: "minNodeCpus"
      <*> o .: "locationHint"
      <*> o .: "availabilityDomain"
      <*> o .: "provisioningModel"
      <*> o .: "instanceTerminationAction"

instance ToJSON Scheduling where
  toJSON Scheduling {..} =
    object
      [ "onHostMaintenance" .= onHostMaintenance
      , "automaticRestart" .= automaticRestart
      , "preemptible" .= preemptible
      , "nodeAffinities" .= nodeAffinities
      , "minNodeCpus" .= minNodeCpus
      , "locationHint" .= locationHint
      , "availabilityDomain" .= availabilityDomain
      , "provisioningModel" .= provisioningModel
      , "instanceTerminationAction" .= instanceTerminationAction
      ]

data InstanceTerminationAction
  = RunDurationRunDuration RunDuration
  deriving (Show, Eq)

instance FromJSON InstanceTerminationAction where
  parseJSON v = RunDurationRunDuration <$> parseJSON v

instance ToJSON InstanceTerminationAction where
  toJSON (RunDurationRunDuration rd) = toJSON rd

data NodeAffinity = NodeAffinity
  { key :: String
  , operator :: String
  , values :: [String]
  }
  deriving (Show, Eq)

instance FromJSON NodeAffinity where
  parseJSON = withObject "NodeAffinity" $ \o ->
    NodeAffinity
      <$> o .: "key"
      <*> o .: "operator"
      <*> o .: "values"

instance ToJSON NodeAffinity where
  toJSON NodeAffinity {..} =
    object
      [ "key" .= key
      , "operator" .= operator
      , "values" .= values
      ]

data Warning = Warning
  { code :: String
  , message :: String
  }
  deriving (Show, Eq)

instance FromJSON Warning where
  parseJSON = withObject "Warning" $ \o ->
    Warning
      <$> o .: "code"
      <*> o .: "message"

instance ToJSON Warning where
  toJSON Warning {..} =
    object
      [ "code" .= code
      , "message" .= message
      ]

-- | Metadata representing a Compute Engine instance
data InstanceMetadata = InstanceMetadata
  { cpuPlatform :: Maybe String
    -- ^ The CPU platform used by this instance
  , labelFingerprint :: Maybe Text
    -- ^ Fingerprint of the label metadata
  , instanceEncryptionKey :: Maybe InstanceEncryptionKey
    -- ^ Encryption key configuration for the instance
  , minCpuPlatform :: Maybe Text
    -- ^ Minimum CPU platform required by the instance
  , guestAccelerators :: Maybe [GuestAccelerator]
    -- ^ List of guest accelerators attached to the instance
  , startRestricted :: Maybe Bool
    -- ^ Whether instance start is restricted due to storage issues
  , deletionProtection :: Maybe Bool
    -- ^ Whether deletion protection is enabled
  , resourcePolicies :: Maybe [Text]
    -- ^ Resource policies applied to the instance
  , sourceMachineImage :: Maybe Text
    -- ^ Source machine image used to create the instance
  , reservationAffinity :: Maybe ReservationAffinity
    -- ^ Reservation affinity configuration
  , hostname :: Maybe Text
    -- ^ Custom hostname assigned to the instance
  , displayDevice :: Maybe DisplayDevice
    -- ^ Display device configuration
  , shieldedInstanceConfig :: Maybe ShieldedInstanceConfig
    -- ^ Shielded VM configuration
  , shieldedInstanceIntegrityPolicy :: Maybe ShieldedInstanceIntegrityPolicy
    -- ^ Integrity policy for shielded instances
  , sourceMachineImageEncryptionKey :: Maybe InstanceEncryptionKey
    -- ^ Encryption key for source machine image
  , confidentialInstanceConfig :: Maybe ConfidentialInstanceConfig
    -- ^ Confidential computing configuration
  , fingerprint :: Maybe Text
    -- ^ Unique fingerprint for resource metadata
  , privateIpv6GoogleAccess :: Maybe Text
    -- ^ Private IPv6 Google access configuration
  , lastStartTimestamp :: Maybe String
    -- ^ Last start timestamp in RFC3339 format
  , lastStopTimestamp :: Maybe String
    -- ^ Last stop timestamp in RFC3339 format
  , lastSuspendedTimestamp :: Maybe String
    -- ^ Last suspension timestamp in RFC3339 format
  , satisfiesPzs :: Maybe Bool
    -- ^ Whether instance satisfies zone separation policy
  , satisfiesPzi :: Maybe Bool
    -- ^ Whether instance satisfies instance separation policy
  , resourceStatus :: Maybe ResourceStatus
    -- ^ Current resource status of the instance
  , networkPerformanceConfig :: Maybe NetworkPerformanceConfig
    -- ^ Network performance configuration
  , keyRevocationActionType :: Maybe Text
    -- ^ Type of key revocation action if applicable
  }
  deriving (Show, Eq)

instance FromJSON InstanceMetadata where
  parseJSON = withObject "InstanceMetadata" $ \o ->
    InstanceMetadata
      <$> o .:? "cpuPlatform"
      <*> o .:? "labelFingerprint"
      <*> o .:? "instanceEncryptionKey"
      <*> o .:? "minCpuPlatform"
      <*> o .:? "guestAccelerators"
      <*> o .:? "startRestricted"
      <*> o .:? "deletionProtection"
      <*> o .:? "resourcePolicies"
      <*> o .:? "sourceMachineImage"
      <*> o .:? "reservationAffinity"
      <*> o .:? "hostname"
      <*> o .:? "displayDevice"
      <*> o .:? "shieldedInstanceConfig"
      <*> o .:? "shieldedInstanceIntegrityPolicy"
      <*> o .:? "sourceMachineImageEncryptionKey"
      <*> o .:? "confidentialInstanceConfig"
      <*> o .:? "fingerprint"
      <*> o .:? "privateIpv6GoogleAccess"
      <*> o .:? "lastStartTimestamp"
      <*> o .:? "lastStopTimestamp"
      <*> o .:? "lastSuspendedTimestamp"
      <*> o .:? "satisfiesPzs"
      <*> o .:? "satisfiesPzi"
      <*> o .:? "resourceStatus"
      <*> o .:? "networkPerformanceConfig"
      <*> o .:? "keyRevocationActionType"

instance ToJSON InstanceMetadata where
  toJSON im =
    object $
      catMaybes
        [ ("cpuPlatform" .=) <$> cpuPlatform im
        , ("labelFingerprint" .=) <$> labelFingerprint im
        , ("instanceEncryptionKey" .=) <$> instanceEncryptionKey im
        , ("minCpuPlatform" .=) <$> minCpuPlatform im
        , ("guestAccelerators" .=) <$> guestAccelerators im
        , ("startRestricted" .=) <$> startRestricted im
        , ("deletionProtection" .=) <$> deletionProtection im
        , ("resourcePolicies" .=) <$> resourcePolicies im
        , ("sourceMachineImage" .=) <$> sourceMachineImage im
        , ("reservationAffinity" .=) <$> reservationAffinity im
        , ("hostname" .=) <$> hostname im
        , ("displayDevice" .=) <$> displayDevice im
        , ("shieldedInstanceConfig" .=) <$> shieldedInstanceConfig im
        , ("shieldedInstanceIntegrityPolicy" .=) <$> shieldedInstanceIntegrityPolicy im
        , ("sourceMachineImageEncryptionKey" .=) <$> sourceMachineImageEncryptionKey im
        , ("confidentialInstanceConfig" .=) <$> confidentialInstanceConfig im
        , ("fingerprint" .=) <$> fingerprint im
        , ("privateIpv6GoogleAccess" .=) <$> privateIpv6GoogleAccess im
        , ("lastStartTimestamp" .=) <$> lastStartTimestamp im
        , ("lastStopTimestamp" .=) <$> lastStopTimestamp im
        , ("lastSuspendedTimestamp" .=) <$> lastSuspendedTimestamp im
        , ("satisfiesPzs" .=) <$> satisfiesPzs im
        , ("satisfiesPzi" .=) <$> satisfiesPzi im
        , ("resourceStatus" .=) <$> resourceStatus im
        , ("networkPerformanceConfig" .=) <$> networkPerformanceConfig im
        , ("keyRevocationActionType" .=) <$> keyRevocationActionType im
        ]

data GuestAccelerator = GuestAccelerator
  { acceleratorType :: Text
  , acceleratorCount :: Int
  }
  deriving (Show, Eq)

instance FromJSON GuestAccelerator where
  parseJSON = withObject "GuestAccelerator" $ \o ->
    GuestAccelerator
      <$> o .: "acceleratorType"
      <*> o .: "acceleratorCount"

instance ToJSON GuestAccelerator where
  toJSON ga =
    object
      [ "acceleratorType" .= acceleratorType ga
      , "acceleratorCount" .= acceleratorCount ga
      ]

data ReservationAffinity = ReservationAffinity
  { consumeReservationType :: Maybe Text
  , key :: Maybe Text
  , values :: Maybe [Text]
  }
  deriving (Show, Eq)

instance FromJSON ReservationAffinity where
  parseJSON = withObject "ReservationAffinity" $ \o ->
    ReservationAffinity
      <$> o .:? "consumeReservationType"
      <*> o .:? "key"
      <*> o .:? "values"

instance ToJSON ReservationAffinity where
  toJSON ReservationAffinity {..} =
    object $
      catMaybes
        [ ("consumeReservationType" .=) <$> consumeReservationType
        , ("key" .=) <$> key
        , ("values" .=) <$> values
        ]

data DisplayDevice = DisplayDevice
  { enableDisplay :: Bool
  }
  deriving (Show, Eq)

instance FromJSON DisplayDevice where
  parseJSON = withObject "DisplayDevice" $ \o ->
    DisplayDevice
      <$> o .: "enableDisplay"

instance ToJSON DisplayDevice where
  toJSON dd = object ["enableDisplay" .= enableDisplay dd]

data ShieldedInstanceConfig = ShieldedInstanceConfig
  { enableSecureBoot :: Bool
  , enableVtpm :: Bool
  , enableIntegrityMonitoring :: Bool
  }
  deriving (Show, Eq)

instance FromJSON ShieldedInstanceConfig where
  parseJSON = withObject "ShieldedInstanceConfig" $ \o ->
    ShieldedInstanceConfig
      <$> o .: "enableSecureBoot"
      <*> o .: "enableVtpm"
      <*> o .: "enableIntegrityMonitoring"

instance ToJSON ShieldedInstanceConfig where
  toJSON sic =
    object
      [ "enableSecureBoot" .= enableSecureBoot sic
      , "enableVtpm" .= enableVtpm sic
      , "enableIntegrityMonitoring" .= enableIntegrityMonitoring sic
      ]

data ShieldedInstanceIntegrityPolicy = ShieldedInstanceIntegrityPolicy
  { policy :: Maybe UpdateAutoLearnPolicy
  }
  deriving (Show, Eq)

instance FromJSON ShieldedInstanceIntegrityPolicy where
  parseJSON = withObject "ShieldedInstanceIntegrityPolicy" $ \o ->
    ShieldedInstanceIntegrityPolicy
      <$> o .:? "policy"

instance ToJSON ShieldedInstanceIntegrityPolicy where
  toJSON sip =
    object $
      catMaybes
        [("policy" .=) <$> policy sip]

data UpdateAutoLearnPolicy = UpdateAutoLearnPolicy {updateAutoLearnPolicy :: Bool}
  deriving (Show, Eq)

instance FromJSON UpdateAutoLearnPolicy where
  parseJSON = withObject "UpdateAutoLearnPolicy" $ \o ->
    UpdateAutoLearnPolicy
      <$> o .: "updateAutoLearnPolicy"

instance ToJSON UpdateAutoLearnPolicy where
  toJSON uap = object ["updateAutoLearnPolicy" .= updateAutoLearnPolicy uap]

data ConfidentialInstanceConfig = ConfidentialInstanceConfig
  { enableConfidentialCompute :: Maybe Bool
  , confidentialInstanceType :: Maybe Text
  }
  deriving (Show, Eq)

instance FromJSON ConfidentialInstanceConfig where
  parseJSON = withObject "ConfidentialInstanceConfig" $ \o ->
    ConfidentialInstanceConfig
      <$> o .:? "enableConfidentialCompute"
      <*> o .:? "confidentialInstanceType"

instance ToJSON ConfidentialInstanceConfig where
  toJSON cic =
    object $
      catMaybes
        [ ("enableConfidentialCompute" .=) <$> enableConfidentialCompute cic
        , ("confidentialInstanceType" .=) <$> confidentialInstanceType cic
        ]

data InstanceEncryptionKey = InstanceEncryptionKey
  { sha256 :: Maybe Text
  , kmsKeyServiceAccount :: Maybe Text
  , rawKey :: Maybe Text
  , rsaEncryptedKey :: Maybe Text
  , kmsKeyName :: Maybe Text
  }
  deriving (Show, Eq)

instance FromJSON InstanceEncryptionKey where
  parseJSON = withObject "InstanceEncryptionKey" $ \o ->
    InstanceEncryptionKey
      <$> o .:? "sha256"
      <*> o .:? "kmsKeyServiceAccount"
      <*> o .:? "rawKey"
      <*> o .:? "rsaEncryptedKey"
      <*> o .:? "kmsKeyName"

instance ToJSON InstanceEncryptionKey where
  toJSON iek =
    object $
      catMaybes
        [ ("sha256" .=) <$> sha256 iek
        , ("kmsKeyServiceAccount" .=) <$> kmsKeyServiceAccount iek
        , ("rawKey" .=) <$> rawKey iek
        , ("rsaEncryptedKey" .=) <$> rsaEncryptedKey iek
        , ("kmsKeyName" .=) <$> kmsKeyName iek
        ]

data NetworkPerformanceConfig = NetworkPerformanceConfig
  { totalEgressBandwidthTier :: Text
  }
  deriving (Show, Eq)

instance FromJSON NetworkPerformanceConfig where
  parseJSON = withObject "NetworkPerformanceConfig" $ \o ->
    NetworkPerformanceConfig
      <$> o .: "totalEgressBandwidthTier"

instance ToJSON NetworkPerformanceConfig where
  toJSON npc = object ["totalEgressBandwidthTier" .= totalEgressBandwidthTier npc]

data ResourceStatus = ResourceStatus
  { onHostMaintenance :: String
  , automaticRestart :: Bool
  , preemptible :: Bool
  , nodeAffinities :: [NodeAffinity]
  , minNodeCpus :: Int
  , locationHint :: String
  , availabilityDomain :: Int
  , provisioningModel :: String
  , instanceTerminationAction :: InstanceTerminationAction
  }
  deriving (Show, Eq)

instance FromJSON ResourceStatus where
  parseJSON = withObject "ResourceStatus" $ \o ->
    ResourceStatus
      <$> o .: "onHostMaintenance"
      <*> o .: "automaticRestart"
      <*> o .: "preemptible"
      <*> o .: "nodeAffinities"
      <*> o .: "minNodeCpus"
      <*> o .: "locationHint"
      <*> o .: "availabilityDomain"
      <*> o .: "provisioningModel"
      <*> o .: "instanceTerminationAction"

instance ToJSON ResourceStatus where
  toJSON ResourceStatus {..} =
    object
      [ "onHostMaintenance" .= onHostMaintenance
      , "automaticRestart" .= automaticRestart
      , "preemptible" .= preemptible
      , "nodeAffinities" .= nodeAffinities
      , "minNodeCpus" .= minNodeCpus
      , "locationHint" .= locationHint
      , "availabilityDomain" .= availabilityDomain
      , "provisioningModel" .= provisioningModel
      , "instanceTerminationAction" .= instanceTerminationAction
      ]

data InstanceList = InstanceList
  { kind :: String
  , id_ :: String
  , items :: Maybe [InstanceMetadata]
  }
  deriving (Show, Eq)

instance FromJSON InstanceList where
  parseJSON = withObject "InstanceList" $ \o ->
    InstanceList
      <$> o .: "kind"
      <*> o .: "id"
      <*> o .: "items"

instance ToJSON InstanceList where
  toJSON InstanceList {..} =
    object
      [ "kind" .= kind
      , "id" .= id_
      , "items" .= items
      ]

-- | Response structure for delete instance operation
data InstanceDeleteResp = InstanceDeleteResp
  { kind :: Maybe String
     -- ^ Type of resource (always "compute#operation")
  , id_ :: Maybe String
        -- ^ Unique identifier for the operation
  , creationTimestamp :: Maybe String
      -- ^ Creation timestamp in RFC3339 format
  , name :: Maybe String
  , zone :: Maybe String
  , clientOperationId :: Maybe String
  , operationType :: Maybe String
  , targetLink :: Maybe String
  , targetId :: Maybe String
  , status :: Maybe String
  , statusMessage :: Maybe String
  , user :: Maybe String
  , progress :: Maybe Int
  , insertTime :: Maybe String
  , startTime :: Maybe String
  , endTime :: Maybe String
  , error_ :: Maybe Error_
  , warnings :: Maybe [Warning]
  , httpErrorStatusCode :: Maybe Int
  , httpErrorMessage :: Maybe String
  , selfLink :: Maybe String
  , region :: Maybe String
  , description :: Maybe String
  , operationGroupId :: Maybe String
  }
  deriving (Show, Eq)

instance FromJSON InstanceDeleteResp where
  parseJSON = withObject "InstanceDeleteResp" $ \o ->
    InstanceDeleteResp
      <$> o .:? "kind"
      <*> o .:? "id"
      <*> o .:? "creationTimestamp"
      <*> o .:? "name"
      <*> o .:? "zone"
      <*> o .:? "clientOperationId"
      <*> o .:? "operationType"
      <*> o .:? "targetLink"
      <*> o .:? "targetId"
      <*> o .:? "status"
      <*> o .:? "statusMessage"
      <*> o .:? "user"
      <*> o .:? "progress"
      <*> o .:? "insertTime"
      <*> o .:? "startTime"
      <*> o .:? "endTime"
      <*> o .:? "error"
      <*> o .:? "warnings"
      <*> o .:? "httpErrorStatusCode"
      <*> o .:? "httpErrorMessage"
      <*> o .:? "selfLink"
      <*> o .:? "region"
      <*> o .:? "description"
      <*> o .:? "operationGroupId"

instance ToJSON InstanceDeleteResp where
  toJSON InstanceDeleteResp {..} =
    object $
      catMaybes
        [ ("kind" .=) <$> kind
        , ("id" .=) <$> id_
        , ("creationTimestamp" .=) <$> creationTimestamp
        , ("name" .=) <$> name
        , ("zone" .=) <$> zone
        , ("clientOperationId" .=) <$> clientOperationId
        , ("operationType" .=) <$> operationType
        , ("targetLink" .=) <$> targetLink
        , ("targetId" .=) <$> targetId
        , ("status" .=) <$> status
        , ("statusMessage" .=) <$> statusMessage
        , ("user" .=) <$> user
        , ("progress" .=) <$> progress
        , ("insertTime" .=) <$> insertTime
        , ("startTime" .=) <$> startTime
        , ("endTime" .=) <$> endTime
        , ("error" .=) <$> error_
        , ("warnings" .=) <$> warnings
        , ("httpErrorStatusCode" .=) <$> httpErrorStatusCode
        , ("httpErrorMessage" .=) <$> httpErrorMessage
        , ("selfLink" .=) <$> selfLink
        , ("region" .=) <$> region
        , ("description" .=) <$> description
        , ("operationGroupId" .=) <$> operationGroupId
        ]

data Error_ = Error_
  { errors :: Maybe [ErrorDetail]
  }
  deriving (Show, Eq)

instance FromJSON Error_ where
  parseJSON = withObject "Error" $ \o -> Error_ <$> o .:? "errors"

instance ToJSON Error_ where
  toJSON e = object $ catMaybes [("errors" .=) <$> errors e]

data ErrorDetail = ErrorDetail
  { code :: Maybe String
  , location :: Maybe String
  , message :: Maybe String
  , errorDetails :: Maybe [DetailedError]
  }
  deriving (Show, Eq)

instance FromJSON ErrorDetail where
  parseJSON = withObject "ErrorDetail" $ \o ->
    ErrorDetail
      <$> o .:? "code"
      <*> o .:? "location"
      <*> o .:? "message"
      <*> o .:? "errorDetails"

instance ToJSON ErrorDetail where
  toJSON ErrorDetail {..} =
    object $
      catMaybes
        [ ("code" .=) <$> code
        , ("location" .=) <$> location
        , ("message" .=) <$> message
        , ("errorDetails" .=) <$> errorDetails
        ]

data DetailedError = DetailedError
  { errorInfo :: Maybe ErrorInfo
  , quotaInfo :: Maybe QuotaInfo
  , help :: Maybe Help
  , localizedMessage :: Maybe LocalizedMessage
  }
  deriving (Show, Eq)

instance FromJSON DetailedError where
  parseJSON = withObject "DetailedError" $ \o ->
    DetailedError
      <$> o .:? "errorInfo"
      <*> o .:? "quotaInfo"
      <*> o .:? "help"
      <*> o .:? "localizedMessage"

instance ToJSON DetailedError where
  toJSON de =
    object $
      catMaybes
        [ ("errorInfo" .=) <$> errorInfo de
        , ("quotaInfo" .=) <$> quotaInfo de
        , ("help" .=) <$> help de
        , ("localizedMessage" .=) <$> localizedMessage de
        ]

data ErrorInfo = ErrorInfo
  { reason :: Maybe String
  , domain :: Maybe String
  }
  deriving (Show, Eq)

instance FromJSON ErrorInfo where
  parseJSON = withObject "ErrorInfo" $ \o ->
    ErrorInfo
      <$> o .:? "reason"
      <*> o .:? "domain"

instance ToJSON ErrorInfo where
  toJSON ei =
    object $
      catMaybes
        [ ("reason" .=) <$> reason ei
        , ("domain" .=) <$> domain ei
        ]

data QuotaInfo = QuotaInfo
  { metricName :: Maybe String
  , limitName :: Maybe String
  , limit :: Maybe Double
  , futureLimit :: Maybe Double
  , rolloutStatus :: Maybe String
  }
  deriving (Show, Eq)

instance FromJSON QuotaInfo where
  parseJSON = withObject "QuotaInfo" $ \o ->
    QuotaInfo
      <$> o .:? "metricName"
      <*> o .:? "limitName"
      <*> o .:? "limit"
      <*> o .:? "futureLimit"
      <*> o .:? "rolloutStatus"

instance ToJSON QuotaInfo where
  toJSON qi =
    object $
      catMaybes
        [ ("metricName" .=) <$> metricName qi
        , ("limitName" .=) <$> limitName qi
        , ("limit" .=) <$> limit qi
        , ("futureLimit" .=) <$> futureLimit qi
        , ("rolloutStatus" .=) <$> rolloutStatus qi
        ]

data Help = Help
  { links :: Maybe [Link]
  }
  deriving (Show, Eq)

instance FromJSON Help where
  parseJSON = withObject "Help" $ \o -> Help <$> o .:? "links"

instance ToJSON Help where
  toJSON h = object $ catMaybes [("links" .=) <$> links h]

data Link = Link
  { description :: Maybe String
  , url :: Maybe String
  }
  deriving (Show, Eq)

instance FromJSON Link where
  parseJSON = withObject "Link" $ \o ->
    Link
      <$> o .:? "description"
      <*> o .:? "url"

instance ToJSON Link where
  toJSON Link {..} =
    object $
      catMaybes
        [ ("description" .=) <$> description
        , ("url" .=) <$> url
        ]

data LocalizedMessage = LocalizedMessage
  { locale :: Maybe String
  , message :: Maybe String
  }
  deriving (Show, Eq)

instance FromJSON LocalizedMessage where
  parseJSON = withObject "LocalizedMessage" $ \o ->
    LocalizedMessage
      <$> o .:? "locale"
      <*> o .:? "message"

instance ToJSON LocalizedMessage where
  toJSON LocalizedMessage {..} =
    object $
      catMaybes
        [ ("locale" .=) <$> locale
        , ("message" .=) <$> message
        ]

data InstanceStartResponse = InstanceStartResponse
  { kind :: Maybe Text
  , id_ :: Maybe Text
  , creationTimestamp :: Maybe Text
  , name :: Maybe Text
  , zone :: Maybe Text
  , clientOperationId :: Maybe Text
  , operationType :: Maybe Text
  , targetLink :: Maybe Text
  , targetId :: Maybe Text
  , status :: Maybe Text
  , statusMessage :: Maybe Text
  , user :: Maybe Text
  }
  deriving (Show, Eq)

instance FromJSON InstanceStartResponse where
  parseJSON = withObject "InstanceStartResponse" $ \o ->
    InstanceStartResponse
      <$> o .:? "kind"
      <*> o .:? "id"
      <*> o .:? "creationTimestamp"
      <*> o .:? "name"
      <*> o .:? "zone"
      <*> o .:? "clientOperationId"
      <*> o .:? "operationType"
      <*> o .:? "targetLink"
      <*> o .:? "targetId"
      <*> o .:? "status"
      <*> o .:? "statusMessage"
      <*> o .:? "user"

instance ToJSON InstanceStartResponse where
  toJSON InstanceStartResponse {..} =
    object $
      catMaybes
        [ ("kind" .=) <$> kind
        , ("id" .=) <$> id_
        , ("creationTimestamp" .=) <$> creationTimestamp
        , ("name" .=) <$> name
        , ("zone" .=) <$> zone
        , ("clientOperationId" .=) <$> clientOperationId
        , ("operationType" .=) <$> operationType
        , ("targetLink" .=) <$> targetLink
        , ("targetId" .=) <$> targetId
        , ("status" .=) <$> status
        , ("statusMessage" .=) <$> statusMessage
        , ("user" .=) <$> user
        ]

data InsertInstanceOps = InsertInstanceOps
  { name :: String
  , machineType :: String
  , disks :: Maybe [Disk]
  , networkInterfaces :: Maybe [NetworkInterface]
  }
  deriving (Show, Eq)

data NetworkInterface = NetworkInterface
  { network :: String
  }
  deriving (Show, Eq)

data InsertInstanceQuery = InsertInstanceQuery
  { requestId :: Maybe String
  , sourceInstanceTemplate :: Maybe String
  }
  deriving (Show, Eq)

instance ToJSON InsertInstanceOps where
  toJSON InsertInstanceOps {..} =
    object
      [ "name" .= name
      , "machineType" .= machineType
      , "disks" .= disks
      , "networkInterfaces" .= networkInterfaces
      ]

instance ToJSON NetworkInterface where
  toJSON ni =
    object
      [ "network" .= network ni
      ]

-- | Represents a disk attached to a Compute Engine instance
data Disk = Disk
  { autoDelete :: Bool
    -- ^ Whether disk should be auto-deleted when instance is deleted
  , boot :: Bool
    -- ^ Whether this is a boot disk
  , deviceName :: String
    -- ^ Unique device name visible to the guest OS
  , initializeParams :: Maybe InitializeParams
    -- ^ Initialization parameters for the disk
  , mode :: String
    -- ^ Access mode (READ_WRITE or READ_ONLY)
  , type_ :: String
    -- ^ Disk type (PERSISTENT or SCRATCH)
  }
  deriving (Show, Eq)

-- | Disk initialization parameters for new disk creation
data InitializeParams = InitializeParams
  { diskSizeGb :: String
    -- ^ Size of the disk in GB (passed as String to match API format)
  , diskType :: String
    -- ^ Full URL of disk type resource
  , labels :: Map.Map String String
    -- ^ Labels to apply to the disk
  , sourceImage :: String
    -- ^ Source image used to create the disk
  }
  deriving (Show, Eq)

instance ToJSON Disk where
  toJSON Disk{..} = object
    [ "autoDelete" .= autoDelete
    , "boot" .= boot
    , "deviceName" .= deviceName
    , "initializeParams" .= initializeParams
    , "mode" .= mode
    , "type" .= type_  -- Map type_ to "type" in JSON
    ]

instance FromJSON Disk where
  parseJSON = withObject "Disk" $ \v -> Disk
    <$> v .: "autoDelete"
    <*> v .: "boot"
    <*> v .: "deviceName"
    <*> v .:? "initializeParams"
    <*> v .: "mode"
    <*> v .: "type"  -- Parse "type" field into type_

instance ToJSON InitializeParams where
  toJSON InitializeParams{..} = object
    [ "diskSizeGb" .= diskSizeGb
    , "diskType" .= diskType
    , "labels" .= labels
    , "sourceImage" .= sourceImage
    ]

instance FromJSON InitializeParams where
  parseJSON = withObject "InitializeParams" $ \v -> InitializeParams
    <$> v .: "diskSizeGb"
    <*> v .: "diskType"
    <*> v .: "labels"
    <*> v .: "sourceImage"


-- ** Instance Operation Functions**

-- | List instances in a given zone
--
-- @
-- listInstances 
--   :: String       -- ^ GCP Project ID
--   -> String       -- ^ Zone name
--   -> Maybe ListInstancesQuery  -- ^ Optional query parameters
--   -> IO (Either String InstanceList)
-- @
--
-- Returns either an error message or an 'InstanceList' containing
-- the collection of instances
listInstances ::
  String -> String -> Maybe ListInstancesQuery -> IO (Either String InstanceList)
listInstances projectId zone_ mbQuery = do
  let queryParams = maybe [] toQueryList mbQuery
  doRequestJSON
    RequestOptions
      { reqMethod = GET
      , reqUrl = googleComputeUrl
      , mbQueryParams = Just queryParams
      , mbReqBody = Nothing
      , mbReqHeaders = Nothing
      , mbReqPath = Just ("/projects/" <> projectId <> "/zones/" <> zone_ <> "/instances")
      }
  where
    toQueryList :: ListInstancesQuery -> [(BS.ByteString, Maybe BS.ByteString)]
    toQueryList q =
      catMaybes
        [ ("filter",) . Just . BS8.pack <$> filter_ q
        , ("maxResults",) . Just . BS8.pack . show <$> maxResults q
        , ("orderBy",) . Just . BS8.pack <$> orderBy q
        , ("pageToken",) . Just . BS8.pack <$> pageToken q
        , ("returnPartialSuccess",) . Just . BS8.pack . show <$> returnPartialSuccess q
        ]

deleteInstance ::
  String -> String -> String -> Maybe RequestIdQuery -> IO (Either String InstanceDeleteResp)
deleteInstance project zone_ name_ mbQuery = do
  let queryParams = maybe [] toQueryList mbQuery
  doRequestJSON
    RequestOptions
      { reqMethod = DELETE
      , reqUrl = googleComputeUrl
      , mbQueryParams = Just queryParams
      , mbReqBody = Nothing
      , mbReqHeaders = Nothing
      , mbReqPath = Just ("/projects/" <> project <> "/zones/" <> zone_ <> "/instances/" <> name_)
      }
  where
    toQueryList :: RequestIdQuery -> [(BS.ByteString, Maybe BS.ByteString)]
    toQueryList RequestIdQuery {..} =
      catMaybes
        [("requestId",) . Just . BS8.pack <$> requestId]

startInstance ::
  String ->
  String ->
  String ->
  Maybe RequestIdQuery ->
  IO (Either String InstanceStartResponse)
startInstance projectId zone_ instanceName mbQuery = do
  let queryParams = maybe [] toQueryList mbQuery
  doRequestJSON
    RequestOptions
      { reqMethod = POST
      , reqUrl = googleComputeUrl
      , mbQueryParams = Just queryParams
      , mbReqBody = Nothing
      , mbReqHeaders = Nothing
      , mbReqPath =
          Just ("/projects/" <> projectId <> "/zones/" <> zone_ <> "/instances/" <> instanceName <> "/start")
      }
  where
    toQueryList :: RequestIdQuery -> [(BS.ByteString, Maybe BS.ByteString)]
    toQueryList RequestIdQuery {..} =
      catMaybes
        [("requestId",) . Just . BS8.pack <$> requestId]

stopInstance ::
  String ->
  String ->
  String ->
  Maybe RequestIdQuery ->
  IO (Either String InstanceStartResponse)
stopInstance projectId zone_ instanceName mbQuery = do
  let queryParams = maybe [] toQueryList mbQuery
  doRequestJSON
    RequestOptions
      { reqMethod = POST
      , reqUrl = googleComputeUrl
      , mbQueryParams = Just queryParams
      , mbReqBody = Nothing
      , mbReqHeaders = Nothing
      , mbReqPath =
          Just ("/projects/" <> projectId <> "/zones/" <> zone_ <> "/instances/" <> instanceName <> "/stop")
      }
  where
    toQueryList :: RequestIdQuery -> [(BS.ByteString, Maybe BS.ByteString)]
    toQueryList RequestIdQuery {..} =
      catMaybes
        [("requestId",) . Just . BS8.pack <$> requestId]

insertInstance ::
  String ->
  String ->
  InsertInstanceOps ->
  Maybe InsertInstanceQuery ->
  IO (Either String InstanceStartResponse)
insertInstance projectId zone_ insertOps mbQuery = do
  let queryParams = maybe [] toQueryList mbQuery
  doRequestJSON
    RequestOptions
      { reqMethod = POST
      , reqUrl = googleComputeUrl
      , mbQueryParams = Just queryParams
      , mbReqBody = Just $ encode insertOps
      , mbReqHeaders = Nothing
      , mbReqPath = Just $ "/projects/" <> projectId <> "/zones/" <> zone_ <> "/instances"
      }
  where
    toQueryList :: InsertInstanceQuery -> [(BS.ByteString, Maybe BS.ByteString)]
    toQueryList InsertInstanceQuery {..} =
      catMaybes
        [ ("requestId",) . Just . BS8.pack <$> requestId
        , ("sourceInstanceTemplate",) . Just . BS8.pack <$> sourceInstanceTemplate
        ]

-- ** Default Query Parameter Constructors**

-- All fields set to 'Nothing', which the API interprets as:
-- * No filtering
-- * Default page size
-- * No sorting
-- * Return full results
defaultListInstancesQuery :: ListInstancesQuery
defaultListInstancesQuery =
  ListInstancesQuery
    { filter_ = Nothing
    , maxResults = Nothing
    , orderBy = Nothing
    , pageToken = Nothing
    , returnPartialSuccess = Nothing
    }

defaultRequestIdQuery :: String -> RequestIdQuery
defaultRequestIdQuery rid = RequestIdQuery {requestId = Just rid}

-- | Create a default instance configuration
--
-- Creates a basic instance configuration with:
-- * 10GB persistent boot disk
-- * Default Debian image
-- * Network interface on default VPC
defaultInsertInstanceOps :: String -> String -> String -> String -> InsertInstanceOps
defaultInsertInstanceOps projectId zone instanceName machineType =
  InsertInstanceOps
    { name = instanceName
    , machineType = "zones/" ++ zone ++ "/machineTypes/" ++ machineType
    , disks = Just [defaultDisk]
    , networkInterfaces = Just [defaultNetworkInterface]
  }
  where
    defaultDisk = Disk
      { autoDelete = True
      , boot = True
      , deviceName = instanceName ++ "-boot"  -- Dynamic, based on instanceName
      , initializeParams = Just defaultInitializeParams
      , mode = "READ_WRITE"
      , type_ = "PERSISTENT"
      }
    defaultInitializeParams = InitializeParams
      { diskSizeGb = "10"
      , diskType = "projects/" ++ projectId ++ "/zones/" ++ zone ++ "/diskTypes/pd-standard"
      , labels = Map.empty
      , sourceImage = "projects/debian-cloud/global/images/debian-12-bookworm-v20250311"
      }
    defaultNetworkInterface = NetworkInterface
      { network = "projects/" ++ projectId ++ "/global/networks/default"
      }
