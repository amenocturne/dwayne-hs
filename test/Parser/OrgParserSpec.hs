{-# LANGUAGE OverloadedStrings #-}

module Parser.OrgParserSpec (spec) where

import Control.Lens
import qualified Data.Set as S
import qualified Data.Text as T
import Data.Time.Calendar (fromGregorian)
import Data.Time.LocalTime (LocalTime (..), TimeOfDay (..))
import Model.OrgMode
import Parser.OrgParser
import Parser.Parser
import Test.Hspec

spec :: Spec
spec = do
  describe "taskLevelParser" $ do
    it "parses heading level correctly" $ do
      let (loc, remainder, result) = runParser taskLevelParser "*** Task title"
      result `shouldBe` ParserSuccess 3
      remainder `shouldBe` " Task title"
      loc `shouldBe` Location 1 3 -- Position after "***"
    it "parses deep nesting level (10 asterisks)" $ do
      let (loc, remainder, result) = runParser taskLevelParser "********** Deep nested task"
      result `shouldBe` ParserSuccess 10
      remainder `shouldBe` " Deep nested task"
      loc `shouldBe` Location 1 10 -- Position after 10 asterisks
    it "fails on invalid heading (no asterisks)" $ do
      let (loc, _, result) = runParser taskLevelParser "Task without asterisks"
      isParserError result `shouldBe` True
      loc `shouldBe` zeroLocation -- Should not advance position on error
    it "fails on empty string" $ do
      let (loc, _, result) = runParser taskLevelParser ""
      isParserError result `shouldBe` True
      loc `shouldBe` zeroLocation

  describe "todoKeyWordParser" $ do
    it "parses TODO keyword" $ do
      let (loc, remainder, result) = runParser todoKeyWordParser "TODO Task"
      result `shouldBe` ParserSuccess "TODO"
      remainder `shouldBe` " Task"
      loc `shouldBe` Location 1 4 -- Position after "TODO"
    it "parses DONE keyword" $ do
      let (loc, remainder, result) = runParser todoKeyWordParser "DONE Task"
      result `shouldBe` ParserSuccess "DONE"
      remainder `shouldBe` " Task"
      loc `shouldBe` Location 1 4 -- Position after "DONE"
    it "parses INBOX keyword" $ do
      let (loc, remainder, result) = runParser todoKeyWordParser "INBOX Task"
      result `shouldBe` ParserSuccess "INBOX"
      remainder `shouldBe` " Task"
      loc `shouldBe` Location 1 5 -- Position after "INBOX"
  describe "priorityParser" $ do
    it "parses priority A correctly" $ do
      let (loc, remainder, result) = runParser priorityParser "[#A]"
      result `shouldBe` ParserSuccess 0
      remainder `shouldBe` ""
      loc `shouldBe` Location 1 4 -- Position after "[#A]"
    it "parses priority B correctly" $ do
      let (loc, remainder, result) = runParser priorityParser "[#B]"
      result `shouldBe` ParserSuccess 1
      remainder `shouldBe` ""
      loc `shouldBe` Location 1 4 -- Position after "[#B]"
    it "parses priority Z correctly" $ do
      let (loc, remainder, result) = runParser priorityParser "[#Z]"
      result `shouldBe` ParserSuccess 25
      remainder `shouldBe` ""
      loc `shouldBe` Location 1 4 -- Position after "[#Z]"
    it "fails on invalid priority format" $ do
      let (loc, _, result) = runParser priorityParser "[A]"
      isParserError result `shouldBe` True
      loc `shouldBe` zeroLocation

    it "fails on priority out of range" $ do
      let (loc, _, result) = runParser priorityParser "[#a]"
      isParserError result `shouldBe` True
      loc `shouldBe` zeroLocation

    it "handles trailing text after priority" $ do
      let (loc, remainder, result) = runParser priorityParser "[#A] Task"
      result `shouldBe` ParserSuccess 0
      remainder `shouldBe` " Task"
      loc `shouldBe` Location 1 4

  describe "splitToTitleAndTags" $ do
    it "parses title without tags" $
      splitToTitleAndTags "Simple task title" `shouldBe` ("Simple task title", [])

    it "parses title with single tag" $
      splitToTitleAndTags "Task with tag :work:" `shouldBe` ("Task with tag", ["work"])

    it "parses title with multiple tags" $
      splitToTitleAndTags "Task with tags :tag1:tag2:tag3:" `shouldBe` ("Task with tags", ["tag1", "tag2", "tag3"])

    it "handles empty tag list" $
      splitToTitleAndTags "Task with empty tags ::" `shouldBe` ("Task with empty tags", [])

    it "handles tags with numbers" $
      splitToTitleAndTags "Task with numbered tags :tag1:tag2:tag123:" `shouldBe` ("Task with numbered tags", ["tag1", "tag2", "tag123"])

    it "handles tags with underscores" $
      splitToTitleAndTags "Task with underscore tags :tag_1:tag_2:" `shouldBe` ("Task with underscore tags", ["tag_1", "tag_2"])

    it "doesn't parse malformed tags (invalid characters)" $
      splitToTitleAndTags "Task with malformed :tag1:TAG2:" `shouldBe` ("Task with malformed :tag1:TAG2:", [])

    it "doesn't parse malformed tags (no trailing colon)" $
      splitToTitleAndTags "Task with malformed :tag1:tag2" `shouldBe` ("Task with malformed :tag1:tag2", [])

    it "handles tag-like text in the middle" $
      splitToTitleAndTags "Task with :middle: text :tag1:tag2:" `shouldBe` ("Task with :middle: text", ["tag1", "tag2"])

    it "handles title text immediately preceding tags" $
      splitToTitleAndTags "Some text:tag1:tag2:" `shouldBe` ("Some text", ["tag1", "tag2"])

    it "handles title with no space before tags" $
      splitToTitleAndTags "Title:tag1:tag2:" `shouldBe` ("Title", ["tag1", "tag2"])

    it "handles an empty string" $
      splitToTitleAndTags "" `shouldBe` ("", [])

    it "handles a string with only tags" $
      splitToTitleAndTags ":tag1:tag2:" `shouldBe` ("", ["tag1", "tag2"])

    it "handles a string with only two colons" $
      splitToTitleAndTags "::" `shouldBe` ("", [])

    it "handles a string with more than two colons" $
      splitToTitleAndTags ":::" `shouldBe` ("", [])

  describe "dateTimeParserReimplemented" $ do
    it "parses simple date" $ do
      let (loc, remainder, result) = runParser dateTimeParserReimplemented "2023-01-15 Sun"
      case result of
        ParserSuccess (OrgTime t r d) -> do
          t `shouldBe` Left (fromGregorian 2023 1 15)
          r `shouldBe` Nothing
          d `shouldBe` Nothing
        ParserFailure err -> fail $ "Parser failed: " ++ err
      remainder `shouldBe` ""
      loc `shouldBe` Location 1 14

    it "parses date with time" $ do
      let (loc, remainder, result) = runParser dateTimeParserReimplemented "2023-01-15 Sun 10:30"
      case result of
        ParserSuccess (OrgTime t r d) -> do
          t `shouldBe` Right (LocalTime (fromGregorian 2023 1 15) (TimeOfDay 10 30 0))
          r `shouldBe` Nothing
          d `shouldBe` Nothing
        ParserFailure err -> fail $ "Parser failed: " ++ err
      remainder `shouldBe` ""
      loc `shouldBe` Location 1 20

    it "parses date with weekly repeater" $ do
      let (loc, remainder, result) = runParser dateTimeParserReimplemented "2023-01-15 Sun +1w"
      case result of
        ParserSuccess (OrgTime t r d) -> do
          t `shouldBe` Left (fromGregorian 2023 1 15)
          r `shouldBe` Just (RepeatInterval NextDate 1 Week)
          d `shouldBe` Nothing
        ParserFailure err -> fail $ "Parser failed: " ++ err
      remainder `shouldBe` ""
      loc `shouldBe` Location 1 18

    it "parses date with future repeater format" $ do
      let (loc, remainder, result) = runParser dateTimeParserReimplemented "2023-01-15 Sun ++2m"
      remainder `shouldBe` ""
      case result of
        ParserSuccess (OrgTime t r d) -> do
          t `shouldBe` Left (fromGregorian 2023 1 15)
          r `shouldBe` Just (RepeatInterval NextFutureDate 2 Month)
          d `shouldBe` Nothing
        ParserFailure err -> fail $ "Parser failed: " ++ err
      loc `shouldBe` Location 1 19

    it "parses date with completion repeater format" $ do
      let (loc, remainder, result) = runParser dateTimeParserReimplemented "2023-01-15 Sun .+3d"
      case result of
        ParserSuccess (OrgTime t r d) -> do
          t `shouldBe` Left (fromGregorian 2023 1 15)
          r `shouldBe` Just (RepeatInterval PlusCompletionDate 3 Day)
          d `shouldBe` Nothing
        ParserFailure err -> fail $ "Parser failed: " ++ err
      remainder `shouldBe` ""
      loc `shouldBe` Location 1 19

    it "parses date with delay" $ do
      let (loc, remainder, result) = runParser dateTimeParserReimplemented "2023-01-15 Sun -1d"
      case result of
        ParserSuccess (OrgTime t r d) -> do
          t `shouldBe` Left (fromGregorian 2023 1 15)
          r `shouldBe` Nothing
          d `shouldBe` Just (DelayInterval AllOccurrences 1 Day)
        ParserFailure err -> fail $ "Parser failed: " ++ err
      remainder `shouldBe` ""
      loc `shouldBe` Location 1 18

    it "parses date with first occurrence delay format" $ do
      let (loc, remainder, result) = runParser dateTimeParserReimplemented "2023-01-15 Sun --2d"
      case result of
        ParserSuccess (OrgTime t r d) -> do
          t `shouldBe` Left (fromGregorian 2023 1 15)
          r `shouldBe` Nothing
          d `shouldBe` Just (DelayInterval FirstOccurrence 2 Day)
        ParserFailure err -> fail $ "Parser failed: " ++ err
      remainder `shouldBe` ""
      loc `shouldBe` Location 1 19

    it "parses date with different time units" $ do
      let units = [("+1h", Hour), ("+1d", Day), ("+1w", Week), ("+1m", Month), ("+1y", Year)]
          testUnit (unitStr, unitType) = do
            let (_, _, result) = runParser dateTimeParserReimplemented (T.pack $ "2023-01-15 Sun " ++ unitStr)
            case result of
              ParserSuccess (OrgTime _ (Just (RepeatInterval _ _ timeUnit)) _) ->
                timeUnit `shouldBe` unitType
              _ -> fail $ "Failed parsing time unit: " ++ unitStr

      mapM_ testUnit units

    it "parses date with repeater and delay" $ do
      let (loc, remainder, result) = runParser dateTimeParserReimplemented "2023-01-15 Sun +1w -2d"
      case result of
        ParserSuccess (OrgTime t r d) -> do
          t `shouldBe` Left (fromGregorian 2023 1 15)
          r `shouldBe` Just (RepeatInterval NextDate 1 Week)
          d `shouldBe` Just (DelayInterval AllOccurrences 2 Day)
        ParserFailure err -> fail $ "Parser failed: " ++ err
      remainder `shouldBe` ""
      loc `shouldBe` Location 1 22

  describe "propertyParser" $ do
    it "parses simple property" $ do
      let (loc, remainder, result) = runParser propertyParser ":id: 123\n"
      result `shouldBe` ParserSuccess ("id", "123")
      remainder `shouldBe` ""
      loc `shouldBe` Location 2 0

    it "parses property with spaces" $ do
      let (loc, remainder, result) = runParser propertyParser ":key:  value with spaces\n"
      result `shouldBe` ParserSuccess ("key", "value with spaces")
      remainder `shouldBe` ""
      loc `shouldBe` Location 2 0

    it "fails on property with empty value" $ do
      let (loc, remainder, result) = runParser propertyParser ":key:\n"
      isParserError result `shouldBe` True
      remainder `shouldBe` ":key:\n"
      loc `shouldBe` zeroLocation

    it "parses property with URL value" $ do
      let (loc, remainder, result) = runParser propertyParser ":url: https://example.com/path?query=value\n"
      result `shouldBe` ParserSuccess ("url", "https://example.com/path?query=value")
      remainder `shouldBe` ""
      loc `shouldBe` Location 2 0

    it "parses multiple properties" $ do
      let input =
            T.strip $
              T.unlines
                [ ":PROPERTIES:",
                  ":id: 123",
                  ":url: https://example.com",
                  ":END:"
                ]
          (loc, remainder, result) = runParser propertiesParser input

      result `shouldBe` ParserSuccess [("id", "123"), ("url", "https://example.com")]
      remainder `shouldBe` ""
      loc `shouldBe` Location 4 5

    it "parses empty properties" $ do
      let input =
            T.strip $
              T.unlines
                [ ":PROPERTIES:",
                  ":END:"
                ]
          (loc, remainder, result) = runParser propertiesParser input

      result `shouldBe` ParserSuccess []
      remainder `shouldBe` ""
      loc `shouldBe` Location 2 5

    it "parses properties with mixed content" $ do
      let input =
            T.strip $
              T.unlines
                [ ":PROPERTIES:",
                  ":id: 123",
                  ":tags: work important",
                  ":url: https://example.com",
                  ":END:"
                ]
          (loc, remainder, result) = runParser propertiesParser input

      result `shouldBe` ParserSuccess [("id", "123"), ("tags", "work important"), ("url", "https://example.com")]
      remainder `shouldBe` ""
      loc `shouldBe` Location 5 5

  describe "descriptionParser" $ do
    it "parses simple description" $ do
      let (loc, remainder, result) = runParser descriptionParser "This is a simple description\n"
      result `shouldBe` ParserSuccess "This is a simple description"
      remainder `shouldBe` ""
      loc `shouldBe` Location 2 0

    it "parses multi-line description" $ do
      let input =
            T.strip $
              T.unlines
                [ "This is the first line",
                  "This is the second line",
                  "This is the third line"
                ]
          (loc, remainder, result) = runParser descriptionParser input

      fmap richTextToPlain result `shouldBe` ParserSuccess input
      remainder `shouldBe` ""
      loc `shouldBe` Location 3 22

    it "stops at next heading" $ do
      let input =
            T.unlines
              [ "This is the description",
                "* TODO Next task"
              ]
          (loc, remainder, result) = runParser descriptionParser input

      fmap richTextToPlain result `shouldBe` ParserSuccess "This is the description"
      remainder `shouldBe` "\n* TODO Next task\n"
      loc `shouldBe` Location 1 23

    it "handles empty description" $ do
      let (loc, remainder, result) = runParser descriptionParser "\n* TODO Next task"
      fmap richTextToPlain result `shouldBe` ParserSuccess ""
      remainder `shouldBe` "\n* TODO Next task"
      loc `shouldBe` Location 1 0

  it "handles description with URL links" $ do
    let input =
          T.strip $
            T.unlines
              [ "Description with http://example.com link",
                "And another https://example.org/path?query=value"
              ]
        (loc, remainder, result) = runParser descriptionParser input

    fmap richTextToPlain result `shouldBe` ParserSuccess input
    remainder `shouldBe` ""
    loc `shouldBe` Location 2 48

  it "handles description with org-mode formatting" $ do
    let input =
          T.strip $
            T.unlines
              [ "Description with *bold* and /italic/ formatting",
                "And [[http://example.com][link]]"
              ]
        expectedPlain =
          T.unlines
            [ "Description with *bold* and /italic/ formatting",
              "And link"
            ]
        (loc, remainder, result) = runParser descriptionParser input

    fmap richTextToPlain result `shouldBe` ParserSuccess (T.strip expectedPlain)
    remainder `shouldBe` ""
    loc `shouldBe` Location 2 32
  describe "brokenDescripitonParser" $ do
    it "parses broken description without properties" $
      do
        let input =
              T.strip $
                T.unlines
                  [ "  https://youtu.be/dQw4w9WgXcQ",
                    "  :PROPERTIES:",
                    "  :CREATED:  [2022-06-13 Mon 11:29]",
                    "  :END:"
                  ]
            (_, _, result) = runParser brokenDescriptionParser input
        fmap richTextToPlain result
        `shouldBe` ParserSuccess "https://youtu.be/dQw4w9WgXcQ"

  describe "anyTaskparser" $ do
    it "parses a minimal task" $ do
      let input = "* TODO Minimal task"
          (loc, remainder, result) = runParser anyTaskparser input
      remainder `shouldBe` ""
      case result of
        ParserSuccess task -> do
          view level task `shouldBe` 1
          view todoKeyword task `shouldBe` "TODO"
          view priority task `shouldBe` Nothing
          view title task `shouldBe` "Minimal task"
          view tags task `shouldBe` (S.fromList [])
          view scheduled task `shouldBe` Nothing
          view deadline task `shouldBe` Nothing
          view closed task `shouldBe` Nothing
          view properties task `shouldBe` []
          view description task `shouldBe` ""
        ParserFailure err -> fail $ "Parser failed: " ++ err
      loc `shouldBe` Location 1 19

    it "parses task with no title" $ do
      let input =
            T.strip $
              T.unlines
                [ "* TODO",
                  ":PROPERTIES:",
                  ":END:"
                ]
          (loc, remainder, result) = runParser anyTaskparser input
      remainder `shouldBe` ""
      case result of
        ParserSuccess task -> do
          view level task `shouldBe` 1
          view todoKeyword task `shouldBe` "TODO"
          view priority task `shouldBe` Nothing
          view title task `shouldBe` ""
          view tags task `shouldBe` (S.fromList [])
          view scheduled task `shouldBe` Nothing
          view deadline task `shouldBe` Nothing
          view closed task `shouldBe` Nothing
          view properties task `shouldBe` []
          view description task `shouldBe` ""
        ParserFailure err -> fail $ "Parser failed: " ++ err
      loc `shouldBe` Location 3 5
