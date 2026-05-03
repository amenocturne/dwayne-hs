{-# LANGUAGE OverloadedStrings #-}

module Core.NullableSpec (spec) where

import Core.Nullable (Nullable (..))
import qualified Data.Set as S
import qualified Data.Text as T
import Data.Time (Day, fromGregorian)
import Model.OrgMode (OrgTime (..), RichText (..))
import Test.Hspec

spec :: Spec
spec = do
  describe "Nullable Int" $ do
    it "nullValue is -1" $ (nullValue :: Int) `shouldBe` (-1)
    it "isNull recognizes -1" $ isNull (-1 :: Int) `shouldBe` True
    it "isNull rejects 0" $ isNull (0 :: Int) `shouldBe` False

  describe "Nullable Text" $ do
    it "nullValue is empty" $ (nullValue :: T.Text) `shouldBe` ""
    it "isNull recognizes empty" $ isNull ("" :: T.Text) `shouldBe` True
    it "isNull rejects non-empty" $ isNull ("hello" :: T.Text) `shouldBe` False

  describe "Nullable [a]" $ do
    it "nullValue is []" $ (nullValue :: [Int]) `shouldBe` []
    it "isNull on []" $ isNull ([] :: [Int]) `shouldBe` True
    it "isNull rejects non-empty list" $ isNull [1 :: Int] `shouldBe` False

  describe "Nullable (Set a)" $ do
    it "nullValue is empty set" $ (nullValue :: S.Set Int) `shouldBe` S.empty
    it "isNull on empty set" $ isNull (S.empty :: S.Set Int) `shouldBe` True

  describe "Nullable OrgTime" $ do
    it "nullValue is the 1970 epoch" $ do
      let OrgTime t _ _ = (nullValue :: OrgTime)
      case t of
        Left day -> day `shouldBe` (fromGregorian 1970 1 1 :: Day)
        Right _ -> expectationFailure "expected Left day"

    it "isNull recognizes the epoch" $
      isNull (nullValue :: OrgTime) `shouldBe` True

    it "isNull rejects a non-epoch date" $ do
      let nonEpoch = OrgTime (Left (fromGregorian 2026 4 29)) Nothing Nothing
      isNull nonEpoch `shouldBe` False

  describe "Nullable RichText" $ do
    it "nullValue is empty" $ (nullValue :: RichText) `shouldBe` RichText []
    it "isNull on empty richtext" $ isNull (RichText []) `shouldBe` True
