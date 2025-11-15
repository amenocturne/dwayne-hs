{-# LANGUAGE MultiParamTypeClasses #-}

module Render.Render where

import Brick
import Tui.ColorScheme (ColorScheme, descriptionAttr, levelAttr, priorityAttr, propertyAttr, tagAttr, timeFieldAttr, todoKeywordAttr)

class RenderTask a b where
  renderCompact :: a -> Widget b
  renderFull :: a -> Widget b
  renderCompactWithColors :: ColorScheme -> a -> Widget b
  renderFullWithColors :: ColorScheme -> a -> Widget b

  -- Default implementations that fall back to non-colored versions
  renderCompactWithColors _ = renderCompact
  renderFullWithColors _ = renderFull
