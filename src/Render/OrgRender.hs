{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Render.OrgRender where

import Brick
import Control.Lens (view)
import Data.Char (ord)
import Data.Maybe
import qualified Data.Text as T
import Data.Time (defaultTimeLocale)
import Data.Time.Format (formatTime)
import GHC.Char (chr)
import Model.Injection
import Model.OrgMode
import Render.Render

-- TODO: factor out common functions from it and Writer
instance RenderTask Task b where
  renderCompact task = titleLine
   where
    titleLine =
      txt $
        T.unwords $
          catMaybes
            [ Just $ T.replicate (view level task) "*"
            , Just $ view todoKeyword task -- TODO: colorcode them
            , view priority task >>= renderPriority
            , Just (view title task)
            , renderTags (view tags task)
            ]

    renderPriority p
      | p >= 0 = Just $ T.concat ["[#", T.singleton (chr $ ord 'A' + p), "]"]
      | otherwise = Nothing

    renderTags [] = Nothing
    renderTags ts = Just $ T.concat [":", T.intercalate ":" ts, ":"]

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
      txtWrap $
        T.unwords $
          catMaybes
            [ Just $ T.replicate (view level task) "*"
            , Just $ view todoKeyword task -- TODO: colorcode them
            , view priority task >>= renderPriority
            , Just $ view title task
            , renderTags (view tags task)
            ]
    timeFieldsLine =
      txt $
        T.unwords $
          mapMaybe
            (\(field, getTime) -> fmap (renderTimeField field) (getTime task))
            [ (orgScheduledField, view scheduled)
            , (orgDeadlineField, view deadline)
            , (orgClosedField, view closed)
            ]

    renderTimeField :: TimeField -> OrgTime -> T.Text
    renderTimeField (TimeField n delim) (OrgTime t r d) =
      T.concat
        [ n
        , " "
        , T.singleton (fst delims)
        , displayOrgTime t
        , maybe "" renderRepeater r
        , maybe "" renderDelay d
        , T.singleton (snd delims)
        ]
     where
      delims :: (Char, Char)
      delims = to delim

    propertiesSection = txt $ T.intercalate "\n" (fmap (\(key, value) -> T.concat [":", key, ": ", value]) (view properties task))

    renderPriority p
      | p >= 0 = Just $ T.concat ["[#", T.singleton (chr $ ord 'A' + p), "]"]
      | otherwise = Nothing

    renderTags [] = Nothing
    renderTags ts = Just $ T.concat [":", T.intercalate ":" ts, ":"]

    displayOrgTime (Left day) = T.pack $ formatTime defaultTimeLocale orgDayFormat day
    displayOrgTime (Right utcTime) = T.pack $ formatTime defaultTimeLocale orgDayTimeFormat utcTime

    renderRepeater :: RepeatInterval -> T.Text
    renderRepeater (RepeatInterval tt v tu) =
      T.concat
        [ to tt
        , T.pack $ show v
        , T.singleton (to tu)
        ]
    renderDelay :: DelayInterval -> T.Text
    renderDelay (DelayInterval tt v tu) =
      T.concat
        [ to tt
        , T.pack $ show v
        , T.singleton (to tu)
        ]
