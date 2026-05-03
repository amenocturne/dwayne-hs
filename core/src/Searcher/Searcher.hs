module Searcher.Searcher where

import qualified Data.Text as T

class Searcher a where
  matches :: T.Text -> a -> Bool
