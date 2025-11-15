{-# LANGUAGE OverloadedStrings #-}

module Parser.StandardParsersSpec (spec) where

import qualified Data.Text as T
import GHC.Base
import Parser.Parser
import Parser.StandardParsers
import Test.Hspec

spec :: Spec
spec = do
  describe "charParser" $ do
    it "parses a matching character" $ do
      let (loc, remainder, result) = runParser (charParser 'a') "abc"
      result `shouldBe` ParserSuccess 'a'
      remainder `shouldBe` "bc"
      loc `shouldBe` Location 1 1

    it "fails on a non-matching character" $ do
      let (loc, remainder, result) = runParser (charParser 'a') "xyz"
      isParserError result `shouldBe` True
      remainder `shouldBe` "xyz"
      loc `shouldBe` zeroLocation

    it "handles empty input" $ do
      let (loc, remainder, result) = runParser (charParser 'a') ""
      isParserError result `shouldBe` True
      remainder `shouldBe` ""
      loc `shouldBe` zeroLocation

    it "updates location on newline" $ do
      let (loc, _, result) = runParser (charParser '\n') "\nabc"
      result `shouldBe` ParserSuccess '\n'
      loc `shouldBe` Location 2 0

  describe "singleCharParser" $ do
    it "parses any character" $ do
      let (loc, remainder, result) = runParser singleCharParser "abc"
      result `shouldBe` ParserSuccess 'a'
      remainder `shouldBe` "bc"
      loc `shouldBe` Location 1 1

    it "handles empty input" $ do
      let (loc, remainder, result) = runParser singleCharParser ""
      isParserError result `shouldBe` True
      remainder `shouldBe` ""
      loc `shouldBe` zeroLocation

    it "parses special characters" $ do
      let (loc, remainder, result) = runParser singleCharParser "#@!"
      result `shouldBe` ParserSuccess '#'
      remainder `shouldBe` "@!"
      loc `shouldBe` Location 1 1

  describe "singleDigitParser" $ do
    it "parses a digit" $ do
      let (loc, remainder, result) = runParser singleDigitParser "123"
      result `shouldBe` ParserSuccess 1
      remainder `shouldBe` "23"
      loc `shouldBe` Location 1 1

    it "fails on a non-digit" $ do
      let (loc, remainder, result) = runParser singleDigitParser "abc"
      isParserError result `shouldBe` True
      remainder `shouldBe` "abc"
      loc `shouldBe` zeroLocation

    it "parses a single digit from a mixed input" $ do
      let (loc, remainder, result) = runParser singleDigitParser "5abc"
      result `shouldBe` ParserSuccess 5
      remainder `shouldBe` "abc"
      loc `shouldBe` Location 1 1

  describe "positiveIntParser" $ do
    it "parses a positive integer" $ do
      let (loc, remainder, result) = runParser positiveIntParser "123abc"
      result `shouldBe` ParserSuccess 123
      remainder `shouldBe` "abc"
      loc `shouldBe` Location 1 3

    it "fails on a non-digit start" $ do
      let (loc, remainder, result) = runParser positiveIntParser "abc"
      isParserError result `shouldBe` True
      remainder `shouldBe` "abc"
      loc `shouldBe` zeroLocation

    it "parses zero" $ do
      let (loc, remainder, result) = runParser positiveIntParser "0abc"
      result `shouldBe` ParserSuccess 0
      remainder `shouldBe` "abc"
      loc `shouldBe` Location 1 1

    it "parses large numbers" $ do
      let (loc, remainder, result) = runParser positiveIntParser "9876543210abc"
      result `shouldBe` ParserSuccess 9876543210
      remainder `shouldBe` "abc"
      loc `shouldBe` Location 1 10

  describe "stringParser" $ do
    it "parses a matching string" $ do
      let (loc, remainder, result) = runParser (stringParser "hello") "hello world"
      result `shouldBe` ParserSuccess "hello"
      remainder `shouldBe` " world"
      loc `shouldBe` Location 1 5

    it "fails on a partial match" $ do
      let (loc, remainder, result) = runParser (stringParser "hello") "hell"
      isParserError result `shouldBe` True
      remainder `shouldBe` "hell"
      loc `shouldBe` zeroLocation

    it "fails on a non-matching string" $ do
      let (loc, remainder, result) = runParser (stringParser "hello") "world"
      isParserError result `shouldBe` True
      remainder `shouldBe` "world"
      loc `shouldBe` zeroLocation

    it "handles empty string to parse" $ do
      let (loc, remainder, result) = runParser (stringParser "") "abc"
      result `shouldBe` ParserSuccess ""
      remainder `shouldBe` "abc"
      loc `shouldBe` Location 1 0

    it "updates location across multiple lines" $ do
      let (loc, remainder, result) = runParser (stringParser "hello\nworld") "hello\nworld!"
      result `shouldBe` ParserSuccess "hello\nworld"
      remainder `shouldBe` "!"
      loc `shouldBe` Location 2 5

  describe "splitParser" $ do
    it "splits string by a function" $ do
      let (loc, remainder, result) = runParser (splitParser (T.span (/= ' '))) "hello world"
      result `shouldBe` ParserSuccess "hello"
      remainder `shouldBe` " world"
      loc `shouldBe` Location 1 5

    it "handles empty input" $ do
      let (loc, remainder, result) = runParser (splitParser (T.span (/= ' '))) ""
      result `shouldBe` ParserSuccess ""
      remainder `shouldBe` ""
      loc `shouldBe` Location 1 0

  describe "takeWhileParser" $ do
    it "takes characters matching a predicate" $ do
      let (loc, remainder, result) = runParser (takeWhileParser (/= ' ')) "hello world"
      result `shouldBe` ParserSuccess "hello"
      remainder `shouldBe` " world"
      loc `shouldBe` Location 1 5

    it "handles empty input" $ do
      let (loc, remainder, result) = runParser (takeWhileParser (/= ' ')) ""
      result `shouldBe` ParserSuccess ""
      remainder `shouldBe` ""
      loc `shouldBe` Location 1 0

    it "returns empty on immediate predicate failure" $ do
      let (loc, remainder, result) = runParser (takeWhileParser (/= ' ')) " hello"
      result `shouldBe` ParserSuccess ""
      remainder `shouldBe` " hello"
      loc `shouldBe` Location 1 0

  describe "takeUntilDelimParser" $ do
    it "takes until a delimiter is found" $ do
      let (loc, remainder, result) = runParser (takeUntilDelimParser " ") "hello world"
      result `shouldBe` ParserSuccess "hello"
      remainder `shouldBe` " world"
      loc `shouldBe` Location 1 5

    it "handles empty input" $ do
      let (loc, remainder, result) = runParser (takeUntilDelimParser " ") ""
      result `shouldBe` ParserSuccess ""
      remainder `shouldBe` ""
      loc `shouldBe` Location 1 0

    it "handles multi-character delimiters" $ do
      let (loc, remainder, result) = runParser (takeUntilDelimParser "-->") "hello--> world"
      result `shouldBe` ParserSuccess "hello"
      remainder `shouldBe` "--> world"
      loc `shouldBe` Location 1 5

    it "returns entire string if delimiter not found" $ do
      let (loc, remainder, result) = runParser (takeUntilDelimParser "xyz") "hello world"
      result `shouldBe` ParserSuccess "hello world"
      remainder `shouldBe` ""
      loc `shouldBe` Location 1 11

  describe "wordParser" $ do
    it "parses a word (letters and underscores)" $ do
      let (loc, remainder, result) = runParser wordParser "hello123 world"
      result `shouldBe` ParserSuccess "hello"
      remainder `shouldBe` "123 world"
      loc `shouldBe` Location 1 5

    it "handles empty input" $ do
      let (loc, remainder, result) = runParser wordParser ""
      result `shouldBe` ParserSuccess ""
      remainder `shouldBe` ""
      loc `shouldBe` Location 1 0

    it "parses words with underscores" $ do
      let (loc, remainder, result) = runParser wordParser "hello_world123"
      result `shouldBe` ParserSuccess "hello_world"
      remainder `shouldBe` "123"
      loc `shouldBe` Location 1 11

  describe "skipBlanksExceptNewLinesParser" $ do
    it "skips spaces and tabs but not newlines" $ do
      let (loc, remainder, result) = runParser skipBlanksExceptNewLinesParser "   \t  hello"
      result `shouldBe` ParserSuccess ()
      remainder `shouldBe` "hello"
      loc `shouldBe` Location 1 6

    it "stops at newlines" $ do
      let (loc, remainder, result) = runParser skipBlanksExceptNewLinesParser "  \n  hello"
      result `shouldBe` ParserSuccess ()
      remainder `shouldBe` "\n  hello"
      loc `shouldBe` Location 1 2

    it "handles empty input" $ do
      let (loc, remainder, result) = runParser skipBlanksExceptNewLinesParser ""
      result `shouldBe` ParserSuccess ()
      remainder `shouldBe` ""
      loc `shouldBe` Location 1 0

  describe "skipBlanksParser" $ do
    it "skips all whitespace including newlines" $ do
      let (loc, remainder, result) = runParser skipBlanksParser "  \n\t  hello"
      result `shouldBe` ParserSuccess ()
      remainder `shouldBe` "hello"
      loc `shouldBe` Location 2 3

    it "handles empty input" $ do
      let (loc, remainder, result) = runParser skipBlanksParser ""
      result `shouldBe` ParserSuccess ()
      remainder `shouldBe` ""
      loc `shouldBe` Location 1 0

  describe "tillTheEndOfStringParser" $ do
    it "parses until the end of line" $ do
      let (loc, remainder, result) = runParser tillTheEndOfStringParser "hello world\nnext line"
      result `shouldBe` ParserSuccess "hello world"
      remainder `shouldBe` "\nnext line"
      loc `shouldBe` Location 1 11

    it "parser empty input" $ do
      let (loc, remainder, result) = runParser tillTheEndOfStringParser ""
      result `shouldBe` ParserSuccess ""
      remainder `shouldBe` ""
      loc `shouldBe` zeroLocation

    it "returns empty string on new line" $ do
      let (loc, remainder, result) = runParser tillTheEndOfStringParser "\n"
      result `shouldBe` ParserSuccess ""
      remainder `shouldBe` "\n"
      loc `shouldBe` zeroLocation

  describe "failOnConditionParser" $ do
    it "succeeds when condition is false" $ do
      let (loc, remainder, result) = runParser (failOnConditionParser (charParser 'a') (/= 'a') "Not an 'a'") "abc"
      result `shouldBe` ParserSuccess 'a'
      remainder `shouldBe` "bc"
      loc `shouldBe` Location 1 1

    it "fails when condition is true" $ do
      let (loc, remainder, result) = runParser (failOnConditionParser (charParser 'a') (== 'a') "Is an 'a'") "abc"
      isParserError result `shouldBe` True
      remainder `shouldBe` "abc"
      loc `shouldBe` zeroLocation

  describe "mapError" $ do
    it "maps error message on failure" $ do
      let (loc, remainder, result) = runParser (mapError ("Custom: " ++) (charParser 'a')) "xyz"
      case result of
        ParserFailure err -> err `shouldContain` "Custom: "
        _ -> fail "Expected a parser error"
      remainder `shouldBe` "xyz"
      loc `shouldBe` zeroLocation

    it "doesn't change success result" $ do
      let (loc, remainder, result) = runParser (mapError ("Custom: " ++) (charParser 'a')) "abc"
      result `shouldBe` ParserSuccess 'a'
      remainder `shouldBe` "bc"
      loc `shouldBe` Location 1 1

  describe "maybeParser" $ do
    it "converts success to Just" $ do
      let (loc, remainder, result) = runParser (maybeParser (charParser 'a')) "abc"
      result `shouldBe` ParserSuccess (Just 'a')
      remainder `shouldBe` "bc"
      loc `shouldBe` Location 1 1

    it "converts failure to Nothing" $ do
      let (loc, remainder, result) = runParser (maybeParser (charParser 'a')) "xyz"
      result `shouldBe` ParserSuccess Nothing
      remainder `shouldBe` "xyz"
      loc `shouldBe` zeroLocation

  describe "unMaybeParser" $ do
    it "converts Just to success" $ do
      let (loc, remainder, result) = runParser (unMaybeParser "Not found" (pure (Just 'a'))) "abc"
      result `shouldBe` ParserSuccess 'a'
      remainder `shouldBe` "abc"
      loc `shouldBe` Location 1 0

    it "converts Nothing to error" $ do
      let (loc, remainder, result) = runParser (unMaybeParser "Not found" (pure Nothing)) "abc"
      case result of
        ParserFailure err -> err `shouldBe` "Not found"
        _ -> fail "Expected a parser error"
      remainder `shouldBe` "abc"
      loc `shouldBe` zeroLocation

  describe "takeParser" $ do
    it "takes a specific number of characters" $ do
      let (loc, remainder, result) = runParser (takeParser 5) "hello world"
      result `shouldBe` ParserSuccess "hello"
      remainder `shouldBe` " world"
      loc `shouldBe` Location 1 5

    it "takes all characters if fewer than requested" $ do
      let (loc, remainder, result) = runParser (takeParser 20) "hello"
      result `shouldBe` ParserSuccess "hello"
      remainder `shouldBe` ""
      loc `shouldBe` Location 1 5

    it "takes none if zero requested" $ do
      let (loc, remainder, result) = runParser (takeParser 0) "hello"
      result `shouldBe` ParserSuccess ""
      remainder `shouldBe` "hello"
      loc `shouldBe` Location 1 0

  describe "manyStrict" $ do
    it "parses multiple occurrences" $ do
      let (loc, remainder, result) = runParser (manyStrict (charParser 'a')) "aaabc"
      result `shouldBe` ParserSuccess "aaa"
      remainder `shouldBe` "bc"
      loc `shouldBe` Location 1 3

    it "handles empty result" $ do
      let (loc, remainder, result) = runParser (manyStrict (charParser 'a')) "bc"
      isParserError result `shouldBe` True
      remainder `shouldBe` "bc"
      loc `shouldBe` zeroLocation

  -- it "fails on partial input" $ do
  --   let (loc, remainder, result) = runParser (manyStrict (stringParser "ab")) "ababc"
  --   result `shouldBe` ParserSuccess ["ab", "ab"]
  --   remainder `shouldBe` "c"
  --   loc `shouldBe` Location 1 4

  describe "tryParser" $ do
    it "succeeds without changing behavior" $ do
      let (loc, remainder, result) = runParser (tryParser (charParser 'a')) "abc"
      result `shouldBe` ParserSuccess 'a'
      remainder `shouldBe` "bc"
      loc `shouldBe` Location 1 1

    it "fails without changing position" $ do
      let (loc, remainder, result) = runParser (tryParser (stringParser "abc")) "abd"
      isParserError result `shouldBe` True
      remainder `shouldBe` "abd"
      loc `shouldBe` zeroLocation

  describe "notConsumingInput" $ do
    it "doesn't consume input on success" $ do
      let (loc, remainder, result) = runParser (notConsumingInput (charParser 'a')) "abc"
      result `shouldBe` ParserSuccess 'a'
      remainder `shouldBe` "abc"
      loc `shouldBe` zeroLocation

    it "doesn't consume input on failure" $ do
      let (loc, remainder, result) = runParser (notConsumingInput (charParser 'a')) "xyz"
      isParserError result `shouldBe` True
      remainder `shouldBe` "xyz"
      loc `shouldBe` zeroLocation

  describe "takeUntilSucceeds" $ do
    it "takes until the stopping parser succeeds" $ do
      let (loc, remainder, result) = runParser (takeUntilSucceeds (charParser '.')) "hello.world"
      result `shouldBe` ParserSuccess "hello"
      remainder `shouldBe` ".world"
      loc `shouldBe` Location 1 5

    it "takes everything if stopping parser never succeeds" $ do
      let (loc, remainder, result) = runParser (takeUntilSucceeds (charParser '.')) "hello world"
      result `shouldBe` ParserSuccess "hello world"
      remainder `shouldBe` ""
      loc `shouldBe` Location 1 11

    it "returns empty if stopping parser immediately succeeds" $ do
      let (loc, remainder, result) = runParser (takeUntilSucceeds (charParser 'h')) "hello"
      result `shouldBe` ParserSuccess ""
      remainder `shouldBe` "hello"
      loc `shouldBe` Location 1 0

    it "correctly calculates location" $ do
      let (loc, remainder, result) = runParser (takeUntilSucceeds (stringParser "world")) "hello world"
      result `shouldBe` ParserSuccess "hello "
      remainder `shouldBe` "world"
      loc `shouldBe` Location 1 6

    it "correctly calculates location with new lines" $ do
      let input =
            T.strip $
              T.unlines
                [ "hello",
                  "world"
                ]
      let (loc, remainder, result) = runParser (takeUntilSucceeds (stringParser "world")) input
      result `shouldBe` ParserSuccess "hello\n"
      remainder `shouldBe` "world"
      loc `shouldBe` Location 2 0

  -- More complex parser combinators
  describe "complex parser combinations" $ do
    it "parses a sequence of characters" $ do
      let parser = (\a b c -> [a, b, c]) <$> charParser 'a' <*> charParser 'b' <*> charParser 'c'
          (loc, remainder, result) = runParser parser "abcdef"
      result `shouldBe` ParserSuccess "abc"
      remainder `shouldBe` "def"
      loc `shouldBe` Location 1 3

    it "tries multiple alternatives" $ do
      let parser = charParser 'a' <|> charParser 'b' <|> charParser 'c'
          (_, _, result1) = runParser parser "abc"
          (_, _, result2) = runParser parser "bcd"
          (_, _, result3) = runParser parser "cde"
          (_, _, result4) = runParser parser "def"
      result1 `shouldBe` ParserSuccess 'a'
      result2 `shouldBe` ParserSuccess 'b'
      result3 `shouldBe` ParserSuccess 'c'
      isParserError result4 `shouldBe` True

    it "handles backtracking with try and then" $ do
      let parser = tryParser (stringParser "ab" *> stringParser "cd") <|> stringParser "ab" *> stringParser "ce"
          (loc, remainder, result) = runParser parser "abce"
      result `shouldBe` ParserSuccess "ce"
      remainder `shouldBe` ""
      loc `shouldBe` Location 1 4
