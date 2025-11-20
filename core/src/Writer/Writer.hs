module Writer.Writer where

import qualified Data.Text as T

class Writer a where
  write :: a -> T.Text
