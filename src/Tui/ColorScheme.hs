{-# LANGUAGE TemplateHaskell #-}

module Tui.ColorScheme where

import Brick (AttrName, attrName)
import Control.Lens
import qualified Data.Map.Strict as M
import qualified Data.Text as T
import Graphics.Vty.Attributes (Color)

data ColorScheme = ColorScheme
  { _highlightBgColor :: Color -- Background color for selected line
  , _todoKeywordColors :: M.Map T.Text Color
  , _priorityColors :: [Color] -- Index 0 = priority A, 1 = priority B, etc.
  , _tagColor :: Color
  , _timeFieldColor :: Color
  , _levelColors :: [Color] -- Colors for different heading levels
  , _propertyColor :: Color
  , _descriptionColor :: Color
  , _defaultColor :: Color
  }

makeLenses ''ColorScheme

-- Attribute names for different UI elements
highlightBgAttr :: AttrName
highlightBgAttr = attrName "highlightBg"

todoKeywordAttr :: T.Text -> AttrName
todoKeywordAttr keyword = attrName ("todo." <> T.unpack keyword)

priorityAttr :: Int -> AttrName
priorityAttr p = attrName ("priority." <> show p)

tagAttr :: AttrName
tagAttr = attrName "tag"

timeFieldAttr :: AttrName
timeFieldAttr = attrName "timeField"

levelAttr :: Int -> AttrName
levelAttr l = attrName ("level." <> show l)

propertyAttr :: AttrName
propertyAttr = attrName "property"

descriptionAttr :: AttrName
descriptionAttr = attrName "description"
