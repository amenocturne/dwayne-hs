{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-type-defaults #-}

module Parser.ParserSpec (spec) where

import Parser.Parser
import Parser.StandardParsers
import Test.Hspec

spec :: Spec
spec = do
  let failure :: ParserResult ()
      failure = ParserFailure "error"
  let ps = ParserSuccess 42
  describe "ParserResult functions" $ do
    it "resultToMaybe converts success to Just" $
      resultToMaybe ps `shouldBe` Just 42

    it "resultToMaybe converts failure to Nothing" $ do
      resultToMaybe failure `shouldBe` Nothing

    it "errorToMaybe converts success to Nothing" $
      errorToMaybe ps `shouldBe` Nothing

    it "errorToMaybe converts failure to Just error" $
      errorToMaybe (ParserFailure "error") `shouldBe` Just "error"

  describe "Parser state validation" $ do
    it "isParserSuccess identifies success correctly" $
      isParserSuccess ps `shouldBe` True

    it "isParserSuccess identifies failure correctly" $
      isParserSuccess (ParserFailure "error") `shouldBe` False

    it "isParserError identifies error correctly" $
      isParserError (ParserFailure "error") `shouldBe` True

    it "isParserError identifies success correctly" $
      isParserError ps `shouldBe` False

  describe "runParser" $ do
    it "returns the parsed result and remaining input" $ do
      let (_, remainder, result) = runParser (charParser 'a') "abc"
      result `shouldBe` ParserSuccess 'a'
      remainder `shouldBe` "bc"
