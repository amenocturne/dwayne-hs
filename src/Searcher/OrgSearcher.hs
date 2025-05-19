{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Searcher.OrgSearcher where

import Data.Char
import qualified Data.Text as T
import Data.Text.Internal.Search as T
import Searcher.Searcher
import Writer.Writer

instance (Writer a) => Searcher a where
  matches query a = if containsUppercase then check task else check (T.toLower task) -- NOTE: smartcase implementation similar to vim's
   where
    task = write a
    check t = not (null (indices query t))
    containsUppercase = T.any isUpper query
