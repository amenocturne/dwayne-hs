module Commands.StatusSpec (spec) where

import Commands.Status (StatusArgsError (..), parseStatusPort, statusUrl)
import Test.Hspec

spec :: Spec
spec =
  describe "Commands.Status" $ do
    it "defaults to localhost port 8080" $ do
      parseStatusPort [] `shouldBe` Right 8080
      statusUrl 8080 `shouldBe` "http://localhost:8080/api/health"

    it "accepts --port for non-default localhost ports" $
      parseStatusPort ["--port", "9090"] `shouldBe` Right 9090

    it "rejects invalid ports" $
      parseStatusPort ["--port", "abc"] `shouldBe` Left (StatusArgsError "Port must be a positive integer")
