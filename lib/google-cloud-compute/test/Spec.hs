{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

import Test.Hspec
import Google.Cloud.Compute.Disk as Disk
import Google.Cloud.Compute.Instance as Instance
import qualified Google.Cloud.Compute.Firewall as Firewall
import Data.Aeson (decode, encode)
import Data.Maybe (isJust)
import qualified Data.ByteString.Lazy.Char8 as BSL

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "Google.Cloud.Compute.Disk" $ do
    context "DiskResponse JSON serialization" $ do
      it "successfully parses valid JSON to DiskResponse" $ do
        let json = "{\"kind\":\"compute#diskList\",\"items\":[],\"nextPageToken\":\"token\",\"selfLink\":\"link\"}"
        let result = decode (BSL.pack json) :: Maybe DiskResponse
        result `shouldSatisfy` isJust

      it "successfully converts DiskResponse to JSON" $ do
        let diskResponse = DiskResponse (Just "compute#diskList") Nothing (Just []) (Just "token") (Just "link") Nothing
        let json = encode diskResponse
        decode json `shouldBe` Just diskResponse

    context "InsertDiskOps default" $ do
      it "creates a default InsertDiskOps" $ do
        let diskOps = defaultInsertOps "my-disk" "100"
        (\InsertDiskOps{..} -> name) diskOps `shouldBe` "my-disk"
        (\InsertDiskOps{..} -> sizeGb) diskOps `shouldBe` "100"
        (\InsertDiskOps{..} -> description) diskOps `shouldBe` Nothing

    context "CreateSnapshotOps default" $ do
      it "creates a default CreateSnapshotOps" $ do
        let snapshotOps = defaultCreateSnapshotOps "my-snapshot"
        (\CreateSnapshotOps{..} -> name) snapshotOps `shouldBe` "my-snapshot"
        (\CreateSnapshotOps{..} -> description) snapshotOps `shouldBe` Nothing

    context "Warning JSON serialization" $ do
      it "serializes and deserializes Warning correctly" $ do
        let warning = Disk.Warning "NO_RESULTS" "No results found" (Just [WarningData "scope" "global"])
        let json = encode warning
        decode json `shouldBe` Just warning

    context "Operation JSON serialization" $ do
      it "successfully parses valid JSON to Operation" $ do
        let json = "{\"id\":\"123\",\"name\":\"operation-1\",\"status\":\"DONE\"}"
        let result = decode (BSL.pack json) :: Maybe Operation
        result `shouldSatisfy` isJust

      it "successfully converts Operation to JSON" $ do
        let operation = Operation (Just "123") (Just "operation-1") Nothing Nothing Nothing (Just "DONE") Nothing Nothing Nothing Nothing Nothing Nothing Nothing
        let json = encode operation
        decode json `shouldBe` Just operation

      describe "JSON Serialization" $ do
        it "correctly serializes and deserializes RunDuration" $ do
          let runDuration = TerminationTime "2023-10-15T10:00:00Z"
          decode (encode runDuration) `shouldBe` Just runDuration

        it "correctly serializes and deserializes OnInstanceTerminationAction" $ do
          let action = DiscardLocalSsd True
          decode (encode action) `shouldBe` Just action

        it "correctly serializes and deserializes Scheduling" $ do
          let scheduling = Scheduling {
                onHostMaintenance = "MIGRATE",
                automaticRestart = True,
                preemptible = False,
                nodeAffinities = [],
                minNodeCpus = 1,
                locationHint = "us-central1",
                availabilityDomain = 0,
                provisioningModel = "STANDARD",
                instanceTerminationAction = RunDurationRunDuration (TerminationTime "2023-10-15T12:00:00Z")
              }
          decode (encode scheduling) `shouldBe` Just scheduling

      describe "Default Query Constructors" $ do
        it "creates a default ListInstancesQuery with all fields set to Nothing" $ do
          let query = defaultListInstancesQuery
          filter_ query `shouldBe` Nothing
          maxResults query `shouldBe` Nothing
          orderBy query `shouldBe` Nothing
          pageToken query `shouldBe` Nothing
          returnPartialSuccess query `shouldBe` Nothing

        it "creates a default InsertInstanceOps with correct settings" $ do
          let ops = defaultInsertInstanceOps "my-project" "us-central1-a" "test-instance" "f1-micro"
          (\InsertInstanceOps{..} -> name) ops `shouldBe` "test-instance"
          machineType ops `shouldBe` "zones/us-central1-a/machineTypes/f1-micro"
          isJust (disks ops) `shouldBe` True
          isJust (networkInterfaces ops) `shouldBe` True

        it "creates a default RequestIdQuery with given requestId" $ do
          let rid = "request-123"
          let query = defaultRequestIdQuery rid
          requestId query `shouldBe` Just rid
      
    describe "Google.Cloud.Compute.Firewall" $ do

        describe "JSON serialization/deserialization" $ do
          it "should serialize and deserialize WarningData" $ do
            let warningData = Firewall.WarningData "some_key" "some_value"
            let json = encode warningData
            decode json `shouldBe` Just warningData

          it "should serialize and deserialize Warning" $ do
            let warning = Firewall.Warning "some_code" "some_message" Nothing
            let json = encode warning
            decode json `shouldBe` Just warning

          it "should serialize and deserialize LogConfig" $ do
            let logConfig = Firewall.LogConfig True (Just "INCLUDE_ALL_METADATA")
            let json = encode logConfig
            decode json `shouldBe` Just logConfig

          it "should serialize and deserialize AllowedFirewall" $ do
            let allowedFirewall = Firewall.AllowedFirewall "tcp" (Just ["80", "443"])
            let json = encode allowedFirewall
            decode json `shouldBe` Just allowedFirewall

          it "should serialize and deserialize DeniedFirewall" $ do
            let deniedFirewall = Firewall.DeniedFirewall "tcp" (Just ["22"])
            let json = encode deniedFirewall
            decode json `shouldBe` Just deniedFirewall

          it "should serialize and deserialize FirewallMeta" $ do
            let firewallMeta = Firewall.FirewallMeta (Just "compute#firewall") (Just "123456") (Just "2021-01-01T00:00:00Z") 
                                          (Just "test-firewall") (Just "A test firewall") 
                                          (Just "global/networks/default") (Just 1000) 
                                          (Just ["0.0.0.0/0"]) (Just ["tag1"]) (Just ["tag2"]) 
                                          (Just [Firewall.AllowedFirewall "tcp" (Just ["80"])]) 
                                          (Just [Firewall.DeniedFirewall "tcp" (Just ["22"])]) 
                                          (Just "INGRESS") (Just (Firewall.LogConfig True (Just "INCLUDE_ALL_METADATA"))) 
                                          (Just False) (Just "selfLink")
            let json = encode firewallMeta
            decode json `shouldBe` Just firewallMeta

          it "should serialize and deserialize FirewallList" $ do
            let firewallList = Firewall.FirewallList (Just "compute#firewallList") (Just "12345") 
                                            (Just [Firewall.FirewallMeta (Just "compute#firewall") (Just "123456") 
                                            (Just "2021-01-01T00:00:00Z") (Just "test-firewall") 
                                            (Just "A test firewall") (Just "global/networks/default") 
                                            (Just 1000) (Just ["0.0.0.0/0"]) (Just ["tag1"]) 
                                            (Just ["tag2"]) (Just [Firewall.AllowedFirewall "tcp" (Just ["80"])] ) 
                                            (Just [Firewall.DeniedFirewall "tcp" (Just ["22"])] ) 
                                            (Just "INGRESS") (Just (Firewall.LogConfig True (Just "INCLUDE_ALL_METADATA"))) 
                                            (Just False) (Just "selfLink")]) 
                                            (Just "nextPageToken") (Just "selfLink") 
                                            (Just (Firewall.Warning "some_code" "some_message" Nothing))
            let json = encode firewallList
            decode json `shouldBe` Just firewallList

          it "should create default CreateFirewallOps" $ do
            let defaultOps = Firewall.defaultCreateFirewallOps "test-firewall"
            (\Firewall.CreateFirewallOps{..} -> name) defaultOps `shouldBe` "test-firewall"
            (\Firewall.CreateFirewallOps{..} -> description) defaultOps `shouldBe` Nothing
            (\Firewall.CreateFirewallOps{..} -> network) defaultOps `shouldBe` Nothing
            (\Firewall.CreateFirewallOps{..} -> priority) defaultOps `shouldBe` Nothing
            (\Firewall.CreateFirewallOps{..} -> sourceRanges) defaultOps `shouldBe` Nothing
            (\Firewall.CreateFirewallOps{..} -> sourceTags) defaultOps `shouldBe` Nothing
            (\Firewall.CreateFirewallOps{..} -> targetTags) defaultOps `shouldBe` Nothing
            (\Firewall.CreateFirewallOps{..} -> allowed) defaultOps `shouldBe` Nothing
            (\Firewall.CreateFirewallOps{..} -> denied) defaultOps `shouldBe` Nothing
            (\Firewall.CreateFirewallOps{..} -> direction) defaultOps `shouldBe` Nothing
            (\Firewall.CreateFirewallOps{..} -> logConfig) defaultOps `shouldBe` Nothing
            (\Firewall.CreateFirewallOps{..} -> disabled) defaultOps `shouldBe` Nothing
