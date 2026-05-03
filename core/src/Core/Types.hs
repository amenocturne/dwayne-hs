{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

-- | Core types re-exported from Model.OrgMode for use in Core.Operations
-- This module provides a clean interface to the core domain types without
-- exposing TUI-specific dependencies.
module Core.Types
  ( -- * Task Types
    Task (..),
    TaskFile (..),
    TaskPointer (..),

    -- * Time Types
    OrgTime (..),
    TimeUnit (..),
    RepeatType (..),
    DelayType (..),
    RepeatInterval (..),
    DelayInterval (..),
    TimeField (..),
    Delimiter (..),

    -- * Lenses
    level,
    todoKeyword,
    priority,
    title,
    tags,
    scheduled,
    deadline,
    createdProp,
    closed,
    properties,
    description,
    name,
    content,
    file,
    taskIndex,

    -- * Constants
    orgTodoKeyWords,
    orgCreatedProperty,
    orgDayTimeFormat,

    -- * File State Type
    FileState,
    ParserResult (..),
    resultToMaybe,

    -- * Store Operations
    TaskStoreOps (..),
  )
where

import Control.Lens (makeLenses)
import Data.Aeson (FromJSON (..), ToJSON (..), object, withObject, (.:), (.=))
import qualified Data.Map.Strict as M
import qualified Data.Text as T
import Model.OrgMode
import Parser.Parser (ParserResult (..), resultToMaybe)

-- | FileState type represents the collection of all task files
type FileState a = M.Map FilePath (ParserResult (TaskFile a))

-- | TaskPointer identifies a specific task within the file system
data TaskPointer = TaskPointer
  { _file :: FilePath,
    _taskIndex :: Int
  }
  deriving (Eq, Show)

makeLenses ''TaskPointer

-- | JSON serialization for TaskPointer
instance ToJSON TaskPointer where
  toJSON (TaskPointer filePath idx) =
    object
      [ "file" .= filePath,
        "taskIndex" .= idx
      ]

instance FromJSON TaskPointer where
  parseJSON = withObject "TaskPointer" $ \v ->
    TaskPointer
      <$> v .: "file"
      <*> v .: "taskIndex"

-- | Backend-agnostic task storage operations.
-- Captured as closures so SystemConfig doesn't need existential types.
data TaskStoreOps a = TaskStoreOps
  { storeLoad :: IO (FileState a),
    storeSave :: FileState a -> IO ()
  }
