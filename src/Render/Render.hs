{-# LANGUAGE MultiParamTypeClasses #-}
module Render.Render where

import Brick

class RenderTask a b where
  renderFull :: a -> Widget b -- TODO: change () to widget names
  renderCompact :: a -> Widget b -- TODO: change () to widget names
