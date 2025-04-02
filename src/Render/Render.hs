{-# LANGUAGE MultiParamTypeClasses #-}
module Render.Render where

import Brick

class RenderTask a b where
  renderFull :: a -> Widget b
  renderCompact :: a -> Widget b
