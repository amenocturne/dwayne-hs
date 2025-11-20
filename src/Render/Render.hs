{-# LANGUAGE MultiParamTypeClasses #-}

module Render.Render where

import Brick
import Tui.ColorScheme (ColorScheme)

class Render a b where
  renderCompact :: ColorScheme -> a -> Widget b
  renderFull :: ColorScheme -> a -> Widget b
