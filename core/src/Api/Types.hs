{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}

-- | API-specific types for the Servant REST API
module Api.Types
  ( -- * HTTP Methods
    ApiMethod (..),

    -- * Request Types
    CaptureRequest (..),
    ChangeKeywordRequest (..),
    ChangePriorityRequest (..),
    EditTaskRequest (..),
    TagRequest (..),
    TaskPointerRequest (..),

    -- * Request Transforms
    requestToTransform,

    -- * Response Types
    TaskWithPointer (..),
    ResponseMetadata (..),
    PaginatedResponse (..),
    TaskNode (..),
    ProjectTreeResponse (..),

    -- * API Bindings
    ApiBinding (..),
  )
where

import Control.Lens ((.~))
import Core.Types (OrgTime, Task, TaskPointer)
import qualified Core.Types as CT
import Data.Aeson (FromJSON (..), ToJSON (..), Value (..), object, withObject, (.:), (.:?), (.=))
import Data.Aeson.Key (Key)
import Data.Aeson.Types (Object, Parser)
import qualified Data.Set as S
import qualified Data.Text as T
import GHC.Generics (Generic)
import Model.OrgMode (plainToRichText)
import Servant (Handler)
import Tui.Types (AppContext)

-- | HTTP methods supported by the API
data ApiMethod
  = GET
  | POST
  | PUT
  | DELETE
  deriving (Eq, Show)

-- | Wrapper type pairing a Task with its pointer for API responses
-- This allows clients to reference specific tasks by their location
data TaskWithPointer = TaskWithPointer
  { twpTask :: Task,
    twpPointer :: TaskPointer
  }
  deriving (Eq, Show)

-- | JSON serialization for TaskWithPointer
-- Response-only type, so we only need ToJSON
instance ToJSON TaskWithPointer where
  toJSON (TaskWithPointer task pointer) =
    object
      [ "task" .= task,
        "pointer" .= pointer
      ]

-- | Response metadata for paginated results
data ResponseMetadata = ResponseMetadata
  { rmTotal :: Int
  }
  deriving (Eq, Show, Generic)

instance ToJSON ResponseMetadata where
  toJSON (ResponseMetadata total) = object ["total" .= total]

-- | Paginated response wrapper
data PaginatedResponse a = PaginatedResponse
  { prData :: [a],
    prMetadata :: ResponseMetadata
  }
  deriving (Eq, Show, Generic)

instance (ToJSON a) => ToJSON (PaginatedResponse a) where
  toJSON (PaginatedResponse items meta) =
    object
      [ "data" .= items,
        "metadata" .= meta
      ]

-- | A node in the project task tree
-- Represents a task with its direct children (subtasks)
data TaskNode = TaskNode
  { tnTask :: Task,
    tnPointer :: TaskPointer,
    tnChildren :: [TaskNode]
  }
  deriving (Eq, Show, Generic)

instance ToJSON TaskNode where
  toJSON (TaskNode task pointer children) =
    object
      [ "task" .= task,
        "pointer" .= pointer,
        "children" .= children
      ]

-- | Response wrapper for project tree endpoint
data ProjectTreeResponse = ProjectTreeResponse
  { ptrRoot :: TaskNode
  }
  deriving (Eq, Show, Generic)

instance ToJSON ProjectTreeResponse where
  toJSON (ProjectTreeResponse root) =
    object ["root" .= root]

-- | Request type for quick capture endpoint
data CaptureRequest = CaptureRequest
  { captureTitle :: T.Text
  }
  deriving (Eq, Show, Generic)

instance FromJSON CaptureRequest

instance ToJSON CaptureRequest

data ChangeKeywordRequest = ChangeKeywordRequest
  { ckrFile :: FilePath,
    ckrTaskIndex :: Int,
    ckrKeyword :: T.Text
  }
  deriving (Eq, Show, Generic)

instance FromJSON ChangeKeywordRequest

instance ToJSON ChangeKeywordRequest

data ChangePriorityRequest = ChangePriorityRequest
  { cprFile :: FilePath,
    cprTaskIndex :: Int,
    cprPriority :: Maybe Int
  }
  deriving (Eq, Show, Generic)

instance FromJSON ChangePriorityRequest

instance ToJSON ChangePriorityRequest

data TagRequest = TagRequest
  { trFile :: FilePath,
    trTaskIndex :: Int,
    trTag :: T.Text
  }
  deriving (Eq, Show, Generic)

instance FromJSON TagRequest

instance ToJSON TagRequest

data TaskPointerRequest = TaskPointerRequest
  { tprFile :: FilePath,
    tprTaskIndex :: Int
  }
  deriving (Eq, Show, Generic)

instance FromJSON TaskPointerRequest

instance ToJSON TaskPointerRequest

-- | Unified edit request for task mutations.
-- Fields use Maybe to indicate "don't change" (Nothing) vs "set value" (Just x).
-- For fields that are themselves optional (priority, scheduled, deadline),
-- we use Maybe (Maybe X) where:
--   Nothing = don't change
--   Just Nothing = clear the field
--   Just (Just x) = set to x
data EditTaskRequest = EditTaskRequest
  { etrFile :: FilePath,
    etrTaskIndex :: Int,
    etrKeyword :: Maybe T.Text,
    etrPriority :: Maybe (Maybe Int),
    etrTitle :: Maybe T.Text,
    etrTags :: Maybe [T.Text],
    etrScheduled :: Maybe (Maybe OrgTime),
    etrDeadline :: Maybe (Maybe OrgTime)
  }
  deriving (Eq, Show)

instance FromJSON EditTaskRequest where
  parseJSON = withObject "EditTaskRequest" $ \v ->
    EditTaskRequest
      <$> v .: "file"
      <*> v .: "taskIndex"
      <*> v .:? "keyword"
      <*> parseMaybeNullable v "priority"
      <*> v .:? "title"
      <*> v .:? "tags"
      <*> parseMaybeNullable v "scheduled"
      <*> parseMaybeNullable v "deadline"

-- | Parse a field that may be absent (don't change), null (clear), or present (set).
-- absent -> Nothing, null -> Just Nothing, value -> Just (Just value)
parseMaybeNullable :: (FromJSON a) => Object -> Key -> Parser (Maybe (Maybe a))
parseMaybeNullable v key = do
  mVal <- v .:? key :: Parser (Maybe Value)
  case mVal of
    Nothing -> pure Nothing
    Just Null -> pure (Just Nothing)
    Just val -> do
      parsed <- parseJSON val
      pure (Just (Just parsed))

instance ToJSON EditTaskRequest where
  toJSON req =
    object $
      [ "file" .= etrFile req,
        "taskIndex" .= etrTaskIndex req
      ]
        ++ maybe [] (\kw -> ["keyword" .= kw]) (etrKeyword req)
        ++ maybe [] (\mp -> ["priority" .= mp]) (etrPriority req)
        ++ maybe [] (\t -> ["title" .= t]) (etrTitle req)
        ++ maybe [] (\ts -> ["tags" .= ts]) (etrTags req)
        ++ maybe [] (\ms -> ["scheduled" .= ms]) (etrScheduled req)
        ++ maybe [] (\md -> ["deadline" .= md]) (etrDeadline req)

-- | Build a Task -> Task transformation from an EditTaskRequest.
-- Composes lens setters for each non-Nothing field.
requestToTransform :: EditTaskRequest -> (Task -> Task)
requestToTransform req =
  applyIf (etrKeyword req) (CT.todoKeyword .~)
    . applyIf (etrPriority req) (CT.priority .~)
    . applyIf (etrTitle req) (\t -> CT.title .~ plainToRichText t)
    . applyIf (etrTags req) (\ts -> CT.tags .~ S.fromList ts)
    . applyIf (etrScheduled req) (CT.scheduled .~)
    . applyIf (etrDeadline req) (CT.deadline .~)

-- | Apply a setter if the value is Just, otherwise identity
applyIf :: Maybe a -> (a -> Task -> Task) -> Task -> Task
applyIf Nothing _ = id
applyIf (Just v) f = f v

-- | API binding for a command
-- Describes how a command maps to an HTTP endpoint
data ApiBinding a = ApiBinding
  { apiEndpoint :: T.Text, -- e.g., "views/inbox"
    apiMethod :: ApiMethod,
    apiHandler :: AppContext a -> Maybe Int -> Maybe Int -> Handler (PaginatedResponse TaskWithPointer)
  }
