{-# LANGUAGE OverloadedStrings #-}

import Test.Hspec
import Google.Cloud.Common.Core
import Data.Aeson (decode)
import qualified Data.ByteString.Lazy.Char8 as BSL
import Data.Maybe (isJust)

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "Google.Cloud.Common.Core" $ do
    context "AccessTokenResp JSON parsing" $ do
      it "successfully parses valid JSON" $ do
        let json = "{\"access_token\":\"ya29.a0AfH6SM...\",\"expires_in\":3599,\"token_type\":\"Bearer\"}"
        let parsed = decode (BSL.pack json) :: Maybe AccessTokenResp
        parsed `shouldSatisfy` isJust

      it "fails to parse invalid JSON" $ do
        let json = "{\"invalid_json\":\"...\"}"
        let parsed = decode (BSL.pack json) :: Maybe AccessTokenResp
        parsed `shouldBe` Nothing

    context "GoogleApplicationCred JSON parsing" $ do
      it "successfully parses valid JSON" $ do
        let json = "{\"type\":\"service_account\",\"project_id\":\"my-project\",\"private_key_id\":\"1234\",\"private_key\":\"-----BEGIN PRIVATE KEY-----\\n...\\n-----END PRIVATE KEY-----\\n\",\"client_email\":\"my-email@my-project.iam.gserviceaccount.com\",\"client_id\":\"123456789\",\"auth_uri\":\"https://accounts.google.com/o/oauth2/auth\",\"token_uri\":\"https://oauth2.googleapis.com/token\",\"auth_provider_x509_cert_url\":\"https://www.googleapis.com/oauth2/v1/certs\",\"client_x509_cert_url\":\"https://www.googleapis.com/robot/v1/metadata/x509/my-email%40my-project.iam.gserviceaccount.com\",\"universe_domain\":\"googleapis.com\"}"
        let parsed = decode (BSL.pack json) :: Maybe GoogleApplicationCred
        parsed `shouldSatisfy` isJust

      it "fails to parse invalid JSON" $ do
        let json = "{\"invalid_json\":\"...\"}"
        let parsed = decode (BSL.pack json) :: Maybe GoogleApplicationCred
        parsed `shouldBe` Nothing

    context "toPath" $ do
      it "concatenates parts with slashes" $ do
        toPath ["part1", "part2", "part3"] `shouldBe` "/part1/part2/part3"

      it "returns empty string for empty list" $ do
        toPath [] `shouldBe` ""
