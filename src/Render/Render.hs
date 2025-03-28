module Render.Render where

import Brick

class RenderTask a where
  renderFull :: a -> Widget () -- TODO: change () to widget names
  renderCompact :: a -> Widget () -- TODO: change () to widget names
