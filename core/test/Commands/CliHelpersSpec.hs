{-# LANGUAGE OverloadedStrings #-}

module Commands.CliHelpersSpec (spec) where

import Commands.CliHelpers (loadFileStateFromEvents)
import qualified Data.Map.Strict as M
import qualified Data.Set as S
import Data.Time (UTCTime (..), fromGregorian, secondsToDiffTime)
import qualified Data.Vector as V
import DB.Connection (initDatabase, withDatabase)
import Events.Projection (fileStateFromEvents)
import Events.Store (insertEvents, selectAllEvents)
import Events.Types (Event (..), genesisEvent)
import Model.OrgMode (RichText (..), Task (..), TaskFile (..), TextNode (..))
import Parser.Parser (ParserResult (..))
import System.Directory (removeFile)
import System.IO.Temp (emptySystemTempFile)
import Test.Hspec

baseTime :: UTCTime
baseTime = UTCTime (fromGregorian 2026 4 29) (secondsToDiffTime 0)

mkTask :: Task
mkTask =
  Task
    { _level = 1,
      _todoKeyword = "INBOX",
      _priority = Nothing,
      _title = RichText [PlainText "test"],
      _tags = S.empty,
      _scheduled = Nothing,
      _deadline = Nothing,
      _createdProp = Nothing,
      _closed = Nothing,
      _properties = [],
      _description = RichText []
    }

spec :: Spec
spec = do
  describe "loadFileStateFromEvents" $ do
    it "returns empty FileState when events table is empty" $ do
      dbPath <- emptySystemTempFile "dwayne-clihelpers-test.db"
      initDatabase dbPath
      fs <- loadFileStateFromEvents dbPath
      M.null fs `shouldBe` True
      removeFile dbPath

    it "matches `currentState` projection for a non-empty events log" $ do
      dbPath <- emptySystemTempFile "dwayne-clihelpers-test.db"
      initDatabase dbPath
      let events =
            [ genesisEvent "/inbox.org" 0 baseTime mkTask,
              genesisEvent "/inbox.org" 1 baseTime (mkTask {_title = RichText [PlainText "second"]}),
              genesisEvent "/projects.org" 0 baseTime (mkTask {_todoKeyword = "PROJECT"})
            ]
      withDatabase dbPath $ \conn -> do
        _ <- insertEvents conn events
        pure ()
      fs <- loadFileStateFromEvents dbPath
      -- expected: same shape as fileStateFromEvents over the freshly read events
      readBack <- withDatabase dbPath selectAllEvents
      let expected = fileStateFromEvents readBack
      fs `shouldBe` expected
      -- and basic shape sanity
      M.size fs `shouldBe` 2
      M.member "/inbox.org" fs `shouldBe` True
      M.member "/projects.org" fs `shouldBe` True
      removeFile dbPath

    it "honors latest-Just-wins through delta events" $ do
      dbPath <- emptySystemTempFile "dwayne-clihelpers-test.db"
      initDatabase dbPath
      let g = genesisEvent "/inbox.org" 0 baseTime mkTask
          later = UTCTime (fromGregorian 2026 4 29) (secondsToDiffTime 3600)
          delta =
            (genesisEvent "/inbox.org" 0 baseTime mkTask)
              { evOccurredAt = later,
                evTodoKeyword = Just "DONE",
                evLevel = Nothing,
                evPriority = Nothing,
                evTitle = Nothing,
                evTags = Nothing,
                evScheduled = Nothing,
                evDeadline = Nothing,
                evCreated = Nothing,
                evClosed = Nothing,
                evProperties = Nothing,
                evDescription = Nothing
              }
      withDatabase dbPath $ \conn -> do
        _ <- insertEvents conn [g, delta]
        pure ()
      fs <- loadFileStateFromEvents dbPath
      case M.lookup "/inbox.org" fs of
        Just (ParserSuccess (TaskFile _ tasks)) -> do
          V.length tasks `shouldBe` 1
          _todoKeyword (V.head tasks) `shouldBe` "DONE"
          _title (V.head tasks) `shouldBe` _title mkTask
        _ -> expectationFailure "expected /inbox.org to be present and parseable"
      removeFile dbPath
