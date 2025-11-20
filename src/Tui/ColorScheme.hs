{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Tui.ColorScheme where

import Brick (AttrName, attrName)
import Control.Lens
import qualified Data.Map.Strict as M
import qualified Data.Text as T
import Graphics.Vty (rgbColor)
import Graphics.Vty.Attributes (Color)

data ColorScheme = ColorScheme
  { _highlightBgColor :: Color, -- Background color for selected line
    _todoKeywordColors :: M.Map T.Text Color,
    _priorityColors :: [Color], -- Index 0 = priority A, 1 = priority B, etc.
    _tagColor :: Color,
    _timeFieldColor :: Color,
    _levelColors :: [Color], -- Colors for different heading levels
    _propertyColor :: Color,
    _descriptionColor :: Color,
    _urlColor :: Color,
    _defaultColor :: Color
  }

makeLenses ''ColorScheme

getColorScheme :: String -> ColorScheme
getColorScheme "default" = defaultColorScheme
getColorScheme _ = defaultColorScheme

defaultColorScheme :: ColorScheme
defaultColorScheme =
  ColorScheme
    { _todoKeywordColors =
        M.fromList
          [ ("INBOX", lavender),
            ("RELEVANT", mauve),
            ("SOMEDAY", maroon),
            ("NOTES", green),
            ("LIST", blue),
            ("WAITING", mauve),
            ("PROJECT", green),
            ("TODO", yellow),
            ("DONE", surface2),
            ("TRASH", surface2),
            ("", textColor)
          ],
      _priorityColors = [red, yellow, blue],
      _tagColor = textColor,
      _timeFieldColor = textColor,
      _levelColors = [yellow, red, green, blue, mauve, teal],
      _propertyColor = textColor,
      _descriptionColor = textColor,
      _urlColor = mauve,
      _defaultColor = textColor,
      _highlightBgColor = highlight
    }
  where
    -- Catppuccin Mocha base
    textColor = rgbColor 205 214 244
    surface1 = rgbColor 73 77 100
    surface2 = rgbColor 88 91 112
    highlight = rgbColor 20 20 100

    -- Accent colors
    lavender = rgbColor 180 190 254
    red = rgbColor 243 139 168
    maroon = rgbColor 235 160 172
    mauve = rgbColor 203 166 247
    flamingo = rgbColor 221 161 161
    pink = rgbColor 245 194 231
    blue = rgbColor 137 180 250
    teal = rgbColor 148 226 213
    green = rgbColor 166 227 161
    yellow = rgbColor 249 226 175

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

urlAttr :: AttrName
urlAttr = attrName "url"
