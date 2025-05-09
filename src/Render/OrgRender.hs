{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Render.OrgRender where

import Brick
import Data.Char (ord)
import Data.List (intercalate)
import Data.Maybe
import qualified Data.Text as T
import Data.Time (defaultTimeLocale)
import Data.Time.Format (formatTime)
import GHC.Char (chr)
import Model.Injection
import Model.OrgMode
import Render.Render
import Control.Lens (view)

-- TODO: factor out common functions from it and Writer
instance RenderTask Task b where
  renderCompact task = titleLine
   where
    titleLine =
      str $
        unwords $
          catMaybes
            [ Just $ replicate (view level task) '*'
            , Just $ T.unpack (view todoKeyword task) -- TODO: colorcode them
            , view priority task >>= renderPriority
            , Just $ T.unpack (view title task)
            , renderTags (view tags task)
            ]

    renderPriority p
      | p >= 0 = Just $ concat ["[#", [chr $ ord 'A' + p], "]"]
      | otherwise = Nothing

    renderTags [] = Nothing
    renderTags ts = Just $ ":" ++ intercalate ":" (fmap T.unpack ts) ++ ":"

  renderFull task =
    vBox $
      catMaybes
        [ Just titleLine
        , Just timeFieldsLine
        , Just $ txt orgPropertiesBegin
        , Just propertiesSection
        , Just $ txt orgPropertiesEnd
        , Just $ txt "\n"
        , Just $ txt $ view description task
        ]
   where
    titleLine =
      strWrap $
        unwords $
          catMaybes
            [ Just $ replicate (view level task) '*'
            , Just $ T.unpack (view todoKeyword task) -- TODO: colorcode them
            , view priority task >>= renderPriority
            , Just $ T.unpack $ view title task
            , renderTags (view tags task)
            ]
    timeFieldsLine =
      str $
        unwords $
          mapMaybe
            (\(field, getTime) -> fmap (renderTimeField field) (getTime task))
            [ (orgScheduledField, view scheduled)
            , (orgDeadlineField, view deadline)
            , (orgClosedField, view closed)
            ]

    renderTimeField :: TimeField -> OrgTime -> [Char]
    renderTimeField (TimeField n delim) (OrgTime t r d) =
      concat
        [ T.unpack n
        , " "
        , [fst delims]
        , displayOrgTime t
        , maybe "" renderRepeater r
        , maybe "" renderDelay d
        , [snd delims]
        ]
     where
      delims :: (Char, Char)
      delims = to delim

    propertiesSection = vBox (fmap (\(key, value) -> str (":" ++ T.unpack key ++ ": " ++ T.unpack value)) (view properties task))

    renderPriority p
      | p >= 0 = Just $ concat ["[#", [chr $ ord 'A' + p], "]"]
      | otherwise = Nothing

    renderTags [] = Nothing
    renderTags ts = Just $ ":" ++ intercalate ":" (fmap T.unpack ts) ++ ":"

    displayOrgTime (Left day) = formatTime defaultTimeLocale orgDayFormat day
    displayOrgTime (Right utcTime) = formatTime defaultTimeLocale orgDayTimeFormat utcTime

    renderRepeater :: RepeatInterval -> String
    renderRepeater (RepeatInterval tt v tu) =
      concat
        [ T.unpack $ to tt
        , show v
        , [to tu]
        ]
    renderDelay :: DelayInterval -> String
    renderDelay (DelayInterval tt v tu) =
      concat
        [ T.unpack $ to tt
        , show v
        , [to tu]
        ]
