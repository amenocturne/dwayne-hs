{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}

-- | API-specific types for the Servant REST API
module Api.Types
  ( -- * HTTP Methods
    ApiMethod (..),

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

import Core.Types (Task, TaskPointer)
import Data.Aeson (ToJSON (..), object, (.=))
import qualified Data.Text as T
import GHC.Generics (Generic)
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

instance ToJSON a => ToJSON (PaginatedResponse a) where
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

-- | API binding for a command
-- Describes how a command maps to an HTTP endpoint
data ApiBinding a = ApiBinding
  { apiEndpoint :: T.Text, -- e.g., "views/inbox"
    apiMethod :: ApiMethod,
    apiHandler :: AppContext a -> Maybe Int -> Maybe Int -> Handler (PaginatedResponse TaskWithPointer)
  }
