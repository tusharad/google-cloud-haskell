{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}

module Google.Cloud.Compute.Instance
  ( InstanceMetadata (..)
  , InstanceDeleteResp (..)
  , listInstances
  , deleteInstance
  , startInstance
  , stopInstance
  )
where

import Data.Aeson
import Data.Text (Text)
import GHC.Generics
import Google.Cloud.Common.Core
import Google.Cloud.Compute.Common

-- Define the union for run_duration
data RunDuration -- MaxRunDuration { seconds :: Int, nanos :: Int }
  = TerminationTime String
  deriving (Show, Generic, ToJSON, FromJSON)

-- Define the union for on_instance_termination_action
data OnInstanceTerminationAction = DiscardLocalSsd {discardLocalSsd :: Bool}
  deriving (Show, Generic, ToJSON, FromJSON)

-- Define the scheduling type with unions
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
  deriving (Show, Generic, ToJSON, FromJSON)

-- Define the instance termination action type with unions
data InstanceTerminationAction -- OnInstanceStopAction { discardLocalSsd :: Bool }
  = RunDurationRunDuration RunDuration
  -- End of list of possible types for union field run_duration.,

  deriving
    ( -- | TerminationTime String
      Show
    , Generic
    , ToJSON
    , FromJSON
    )

-- Define the node affinity type with unions
data NodeAffinity = NodeAffinity
  { key :: String
  , operator :: String
  , values :: [String]
  }
  deriving (Show, Generic, ToJSON, FromJSON)

-- Define the warning type
data Warning = Warning
  { code :: String
  , message :: String
  -- data :: [(String, String)]
  }
  deriving (Show, Generic, ToJSON, FromJSON)

-- Define the main object type with unions
data InstanceMetadata = InstanceMetadata
  { cpuPlatform :: Maybe String
  , -- labels :: Map Text Text,
    -- params :: Map Text Text,
    labelFingerprint :: Maybe Text
  , instanceEncryptionKey :: Maybe InstanceEncryptionKey
  , minCpuPlatform :: Maybe Text
  , guestAccelerators :: Maybe [GuestAccelerator]
  , startRestricted :: Maybe Bool
  , deletionProtection :: Maybe Bool
  , resourcePolicies :: Maybe [Text]
  , sourceMachineImage :: Maybe Text
  , reservationAffinity :: Maybe ReservationAffinity
  , hostname :: Maybe Text
  , displayDevice :: Maybe DisplayDevice
  , shieldedInstanceConfig :: Maybe ShieldedInstanceConfig
  , shieldedInstanceIntegrityPolicy :: Maybe ShieldedInstanceIntegrityPolicy
  , sourceMachineImageEncryptionKey :: Maybe InstanceEncryptionKey
  , confidentialInstanceConfig :: Maybe ConfidentialInstanceConfig
  , fingerprint :: Maybe Text
  , privateIpv6GoogleAccess :: Maybe Text
  , -- advancedMachineFeatures :: AdvancedMachineFeatures,
    lastStartTimestamp :: Maybe String
  , lastStopTimestamp :: Maybe String
  , lastSuspendedTimestamp :: Maybe String
  , satisfiesPzs :: Maybe Bool
  , satisfiesPzi :: Maybe Bool
  , resourceStatus :: Maybe ResourceStatus
  , networkPerformanceConfig :: Maybe NetworkPerformanceConfig
  , keyRevocationActionType :: Maybe Text
  }
  deriving (Show, Generic, ToJSON, FromJSON)

-- Define the guest accelerator type
data GuestAccelerator = GuestAccelerator
  { acceleratorType :: Text
  , acceleratorCount :: Int
  }
  deriving (Show, Generic, ToJSON, FromJSON)

-- Define the reservation affinity type with unions
data ReservationAffinity = ReservationAffinity
  { consumeReservationType :: Maybe Text
  , key :: Maybe Text
  , values :: Maybe [Text]
  }
  deriving (Show, Generic, ToJSON, FromJSON)

-- Define the display device type
data DisplayDevice = DisplayDevice
  { enableDisplay :: Bool
  }
  deriving (Show, Generic, ToJSON, FromJSON)

-- Define the shielded instance config type with unions
data ShieldedInstanceConfig = ShieldedInstanceConfig
  { enableSecureBoot :: Bool
  , enableVtpm :: Bool
  , enableIntegrityMonitoring :: Bool
  }
  deriving (Show, Generic, ToJSON, FromJSON)

-- Define the shielded instance integrity policy type with unions
data ShieldedInstanceIntegrityPolicy = ShieldedInstanceIntegrityPolicy
  { policy :: Maybe UpdateAutoLearnPolicy
  }
  -- End of list of possible types for union field policy.
  deriving (Show, Generic, ToJSON, FromJSON)

-- Define the update auto learn policy type
data UpdateAutoLearnPolicy = UpdateAutoLearnPolicy {updateAutoLearnPolicy :: Bool}
  deriving (Show, Generic, ToJSON, FromJSON)

-- Define the confidential instance config type with unions
data ConfidentialInstanceConfig = ConfidentialInstanceConfig
  { enableConfidentialCompute :: Maybe Bool
  , confidentialInstanceType :: Maybe Text
  }
  deriving (Show, Generic, ToJSON, FromJSON)

-- Define the instance encryption key type
data InstanceEncryptionKey = InstanceEncryptionKey
  { sha256 :: Maybe Text
  , kmsKeyServiceAccount :: Maybe Text
  , rawKey :: Maybe Text
  , rsaEncryptedKey :: Maybe Text
  , kmsKeyName :: Maybe Text
  }
  deriving (Show, Generic, ToJSON, FromJSON)

-- Define the network performance config type with unions
data NetworkPerformanceConfig = NetworkPerformanceConfig
  { totalEgressBandwidthTier :: Text
  }
  deriving (Show, Generic, ToJSON, FromJSON)

-- Define the resource status type
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
  deriving (Show, Generic, ToJSON, FromJSON)

data InstanceList = InstanceList
  { kind :: String
  , id :: String
  , items :: [InstanceMetadata]
  }
  deriving (Show, Generic, FromJSON, ToJSON)

listInstances :: String -> String -> IO (Either String InstanceList)
listInstances projectId zone_ = do
  doRequestJSON
    RequestOptions
      { reqMethod = GET
      , reqUrl = googleComputeUrl
      , mbQueryParams = Nothing
      , mbReqBody = Nothing
      , mbReqHeaders = Nothing
      , mbReqPath =
          Just $
            mconcat
              [ "/"
              , "projects"
              , "/"
              , projectId
              , "/"
              , "zones"
              , "/"
              , zone_
              , "/"
              , "instances"
              ]
      }

data InstanceDeleteResp = InstanceDeleteResp
  { kind :: Maybe String
  , id :: Maybe String
  , creationTimestamp :: Maybe String
  , name :: Maybe String
  , zone :: Maybe String
  , clientOperationId :: Maybe String
  , operationType :: Maybe String
  , targetLink :: Maybe String
  , targetId :: Maybe String
  , status :: Maybe String -- Assuming enum as String
  , statusMessage :: Maybe String
  , user :: Maybe String
  , progress :: Maybe Int
  , insertTime :: Maybe String
  , startTime :: Maybe String
  , endTime :: Maybe String
  , error :: Maybe Error
  , warnings :: Maybe [Warning]
  , httpErrorStatusCode :: Maybe Int
  , httpErrorMessage :: Maybe String
  , selfLink :: Maybe String
  , region :: Maybe String
  , description :: Maybe String
  , operationGroupId :: Maybe String
  -- , setCommonInstanceMetadataOperationMetadata :: Maybe SetCommonInstanceMetadataOperationMetadata
  -- , instancesBulkInsertOperationMetadata :: Maybe InstancesBulkInsertOperationMetadata
  }
  deriving (Show, Generic)

instance FromJSON InstanceDeleteResp
instance ToJSON InstanceDeleteResp

data Error = Error
  { errors :: Maybe [ErrorDetail]
  }
  deriving (Show, Generic)

instance FromJSON Error
instance ToJSON Error

data ErrorDetail = ErrorDetail
  { code :: Maybe String
  , location :: Maybe String
  , message :: Maybe String
  , errorDetails :: Maybe [DetailedError]
  }
  deriving (Show, Generic)

instance FromJSON ErrorDetail
instance ToJSON ErrorDetail

data DetailedError = DetailedError
  { errorInfo :: Maybe ErrorInfo
  , quotaInfo :: Maybe QuotaInfo
  , help :: Maybe Help
  , localizedMessage :: Maybe LocalizedMessage
  }
  deriving (Show, Generic)

instance FromJSON DetailedError
instance ToJSON DetailedError

data ErrorInfo = ErrorInfo
  { reason :: Maybe String
  , domain :: Maybe String
  -- , metadatas :: Maybe (Map String String)
  }
  deriving (Show, Generic)

instance FromJSON ErrorInfo
instance ToJSON ErrorInfo

data QuotaInfo = QuotaInfo
  { metricName :: Maybe String
  , limitName :: Maybe String
  , -- , dimensions :: Maybe (Map String String)
    limit :: Maybe Double
  , futureLimit :: Maybe Double
  , rolloutStatus :: Maybe String -- Assuming enum as String
  }
  deriving (Show, Generic)

instance FromJSON QuotaInfo
instance ToJSON QuotaInfo

data Help = Help
  { links :: Maybe [Link]
  }
  deriving (Show, Generic)

instance FromJSON Help
instance ToJSON Help

data Link = Link
  { description :: Maybe String
  , url :: Maybe String
  }
  deriving (Show, Generic)

instance FromJSON Link
instance ToJSON Link

data LocalizedMessage = LocalizedMessage
  { locale :: Maybe String
  , message :: Maybe String
  }
  deriving (Show, Generic)

instance FromJSON LocalizedMessage
instance ToJSON LocalizedMessage

data PerLocationOperation = PerLocationOperation
  { state :: Maybe String -- Assuming enum as String
  , error :: Maybe LocationOperationError
  }
  deriving (Show, Generic)

instance FromJSON PerLocationOperation
instance ToJSON PerLocationOperation

data LocationOperationError = LocationOperationError
  { code :: Maybe Int
  , message :: Maybe String
  -- , details :: Maybe [ErrorDetailItem]
  }
  deriving (Show, Generic)

instance FromJSON LocationOperationError
instance ToJSON LocationOperationError

data PerLocationStatus = PerLocationStatus
  { status :: Maybe String -- Assuming enum as String
  , targetVmCount :: Maybe Int
  , createdVmCount :: Maybe Int
  , failedToCreateVmCount :: Maybe Int
  , deletedVmCount :: Maybe Int
  }
  deriving (Show, Generic)

instance FromJSON PerLocationStatus
instance ToJSON PerLocationStatus

-- Function to delete an instance
deleteInstance :: String -> String -> String -> IO (Either String InstanceDeleteResp)
deleteInstance project zone_ name_ =
  doRequestJSON $
    RequestOptions
      { reqMethod = DELETE
      , reqUrl = googleComputeUrl
      , mbReqPath = Just ("/projects/" <> project <> "/zones/" <> zone_ <> "/instances/" <> name_)
      , mbReqBody = Nothing
      , mbReqHeaders = Nothing
      , mbQueryParams = Nothing
      }

data InstanceStartResponse = InstanceStartResponse
  { kind :: Maybe Text
  , id :: Maybe Text
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
  deriving (Show, Generic)

instance FromJSON InstanceStartResponse

-- Function to start an instance in Google Compute Engine
startInstance :: String -> String -> String -> IO (Either String InstanceStartResponse)
startInstance projectId zone_ instanceName =
  doRequestJSON
    RequestOptions
      { reqMethod = POST
      , reqUrl = googleComputeUrl
      , mbQueryParams = Nothing
      , mbReqBody = Nothing
      , mbReqHeaders = Nothing
      , mbReqPath =
          Just $
            "/"
              <> "projects/"
              <> projectId
              <> "/zones/"
              <> zone_
              <> "/instances/"
              <> instanceName
              <> "/start"
      }

-- Function to start an instance in Google Compute Engine
stopInstance :: String -> String -> String -> IO (Either String InstanceStartResponse)
stopInstance projectId zone_ instanceName =
  doRequestJSON
    RequestOptions
      { reqMethod = POST
      , reqUrl = googleComputeUrl
      , mbQueryParams = Nothing
      , mbReqBody = Nothing
      , mbReqHeaders = Nothing
      , mbReqPath =
          Just $
            "/"
              <> "projects/"
              <> projectId
              <> "/zones/"
              <> zone_
              <> "/instances/"
              <> instanceName
              <> "/stop"
      }

{-
data InsertInstanceOps = InsertInstanceOps {
    name :: String
  , description :: Maybe String
  , canIpForward :: Bool
  , machineType :: String
} deriving (Eq, Show, FromJSON, ToJSON, Generic)

newtype Disks = Disks [Disk]
  deriving (Eq, Show, FromJSON, ToJSON, Generic)

insertInstance :: String -> String -> InsertInstanceOps -> IO (Either String ())
insertInstance projectId zone insertInstanceOps = do
  doRequestJSON
    RequestOptions
      { reqMethod = POST
      , reqUrl = googleComputeUrl
      , mbQueryParams = Nothing
      , mbReqBody = Just $ encode insertInstanceOps
      , mbReqHeaders = Nothing
      , mbReqPath = Just $ mconcat [
            "/"
           , "projects"
           , "/"
           , projectId
           , "/"
           , "zones"
           , "/"
           , zone
           , "/"
           , "instances"
        ]
      }
      -}
