{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}
module Searcher.OrgSearcher where

import qualified Data.Text as T
import Writer.Writer
import Searcher.Searcher
import Data.Text.Internal.Search as T

instance (Writer a) => Searcher a where
  matches query a = not (null (indices query (write a)))
