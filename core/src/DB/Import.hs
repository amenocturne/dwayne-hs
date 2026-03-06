{-# LANGUAGE OverloadedStrings #-}

module DB.Import
  ( importFileState,
    importTask,
    serializeTags,
    serializeProperties,
    serializeOrgTime,
  )
where

import Data.Aeson (encode, toJSON)
import qualified Data.ByteString.Lazy as BL
import qualified Data.Map.Strict as M
import qualified Data.Set as S
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Data.Time (defaultTimeLocale, formatTime)
import qualified Data.Vector as V
import Database.SQLite.Simple
import Database.SQLite.Simple.Types ((:.) (..))
import Model.OrgMode (OrgTime (..), Task (..), TaskFile (..), richTextToPlain)
import Parser.Parser (ParserResult (..))

type FileState a = M.Map FilePath (ParserResult (TaskFile a))

importFileState :: Connection -> FileState Task -> IO Int
importFileState conn fs = do
  execute_ conn "DELETE FROM tasks"
  let files = M.toList fs
  counts <- mapM (importFile conn) files
  pure (sum counts)

importFile :: Connection -> (FilePath, ParserResult (TaskFile Task)) -> IO Int
importFile conn (fp, ParserSuccess (TaskFile _ tasks)) = do
  let indexed = zip [0 ..] (V.toList tasks)
  mapM_ (\(idx, task) -> importTask conn fp idx task) indexed
  pure (length indexed)
importFile _ (_, ParserFailure _) = pure 0

importTask :: Connection -> FilePath -> Int -> Task -> IO ()
importTask conn fp idx task =
  execute
    conn
    "INSERT INTO tasks (file_path, task_index, level, todo_keyword, priority, \
    \title, tags, scheduled, deadline, created, closed, properties, description) \
    \VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?)"
    ( ( fp,
        idx,
        _level task,
        _todoKeyword task,
        _priority task,
        richTextToPlain (_title task),
        serializeTags (_tags task)
      )
        :. ( fmap serializeOrgTime (_scheduled task),
             fmap serializeOrgTime (_deadline task),
             fmap serializeOrgTime (_createdProp task),
             fmap serializeOrgTime (_closed task),
             serializeProperties (_properties task),
             richTextToPlain (_description task)
           )
    )

serializeTags :: S.Set T.Text -> T.Text
serializeTags = decodeUtf8Lazy . encode . toJSON . S.toList

serializeProperties :: [(T.Text, T.Text)] -> T.Text
serializeProperties = decodeUtf8Lazy . encode . toJSON

serializeOrgTime :: OrgTime -> T.Text
serializeOrgTime (OrgTime timeVal _ _) = case timeVal of
  Left day -> T.pack (formatTime defaultTimeLocale "%Y-%m-%d" day)
  Right lt -> T.pack (formatTime defaultTimeLocale "%Y-%m-%d %H:%M:%S" lt)

decodeUtf8Lazy :: BL.ByteString -> T.Text
decodeUtf8Lazy = TE.decodeUtf8 . BL.toStrict
