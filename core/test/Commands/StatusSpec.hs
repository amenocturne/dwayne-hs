module Commands.StatusSpec (spec) where

import Commands.Status
  ( StatusArgsError (..),
    StatusTarget (..),
    healthUrl,
    parseStatusArgs,
    statusUrl,
  )
import Test.Hspec

spec :: Spec
spec =
  describe "Commands.Status" $ do
    it "defaults to localhost port 8080" $ do
      parseStatusArgs [] `shouldBe` Right (LocalStatus 8080)
      statusUrl 8080 `shouldBe` "http://localhost:8080/api/health"

    it "accepts --port for non-default localhost ports" $
      parseStatusArgs ["--port", "9090"] `shouldBe` Right (LocalStatus 9090)

    it "accepts --remote for credential-backed status checks" $
      parseStatusArgs ["--remote"] `shouldBe` Right RemoteStatus

    it "accepts --url and normalizes it to the health endpoint" $ do
      parseStatusArgs ["--url", "https://dwayne.example.com/"]
        `shouldBe` Right (UrlStatus "https://dwayne.example.com/api/health")
      healthUrl "https://dwayne.example.com"
        `shouldBe` "https://dwayne.example.com/api/health"

    it "rejects invalid ports" $
      parseStatusArgs ["--port", "abc"] `shouldBe` Left (StatusArgsError "Port must be a positive integer")
