{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Render.OrgRender where

import Brick
import Control.Lens (view)
import Data.Char (ord)
import qualified Data.Map.Strict as M
import Data.Maybe
import qualified Data.Set as S
import qualified Data.Text as T
import GHC.Char (chr)
import Graphics.Vty.Attributes (defAttr)
import Model.Injection
import Model.OrgFormat
import Model.OrgMode
import Render.Render
import Tui.ColorScheme (ColorScheme, descriptionAttr, levelAttr, priorityAttr, propertyAttr, tagAttr, timeFieldAttr, todoKeywordAttr)

instance RenderTask Task b where
  renderCompact task = txt $ formatHeaderLine level' todoKw priority' title' tags'
    where
      level' = view level task
      todoKw = view todoKeyword task
      priority' = view priority task
      title' = view title task
      tags' = view tags task

  renderCompactWithColors scheme task =
    hBox $
      catMaybes
        [ Just $ withAttr (levelAttr level') $ txt $ formatStars level',
          Just $ txt " ",
          Just $ withAttr (todoKeywordAttr todoKw) $ txt todoKw,
          Just $ txt " ",
          case priority' of
            Just p -> fmap (\t -> withAttr (priorityAttr p) $ txt $ t <> " ") (formatPriority p)
            Nothing -> Nothing,
          Just $ txt title',
          case formatTags (S.toList tags') of
            "" -> Nothing
            t -> Just $ withAttr tagAttr $ txt $ " " <> t
        ]
    where
      level' = view level task
      todoKw = view todoKeyword task
      priority' = view priority task
      title' = view title task
      tags' = view tags task

  renderFull task =
    vBox $
      catMaybes
        [ Just titleLine,
          Just timeFieldsLine,
          Just $ txt orgPropertiesBegin,
          Just propertiesSection,
          Just $ txt orgPropertiesEnd,
          Just $ txt "\n",
          Just $ txtWrap $ view description task
        ]
    where
      titleLine = txtWrap $ formatHeaderLine level' todoKw priority' title' tags'

      timeFieldsLine =
        txt $
          T.unwords $
            mapMaybe
              (\(field, getTime) -> fmap (formatTimeField field) (getTime task))
              [ (orgScheduledField, view scheduled),
                (orgDeadlineField, view deadline),
                (orgClosedField, view closed)
              ]

      propertiesSection = txt $ T.intercalate "\n" (fmap (\(key, value) -> T.concat [":", key, ": ", value]) (view properties task))

      level' = view level task
      todoKw = view todoKeyword task
      priority' = view priority task
      title' = view title task
      tags' = view tags task

  renderFullWithColors scheme task =
    vBox $
      catMaybes
        [ Just titleLineColored,
          Just timeFieldsLineColored,
          Just $ withAttr propertyAttr $ txt orgPropertiesBegin,
          Just propertiesSectionColored,
          Just $ withAttr propertyAttr $ txt orgPropertiesEnd,
          Just $ txt "\n",
          Just $ withAttr descriptionAttr $ txt $ view description task
        ]
    where
      titleLineColored = txtWrap $ formatHeaderLine level' todoKw priority' title' tags'

      timeFieldsLineColored =
        withAttr timeFieldAttr $
          txt $
            T.unwords $
              mapMaybe
                (\(field, getTime) -> fmap (formatTimeField field) (getTime task))
                [ (orgScheduledField, view scheduled),
                  (orgDeadlineField, view deadline),
                  (orgClosedField, view closed)
                ]

      propertiesSectionColored =
        withAttr propertyAttr $
          txt $
            T.intercalate "\n" (fmap (\(key, value) -> T.concat [":", key, ": ", value]) (view properties task))

      level' = view level task
      todoKw = view todoKeyword task
      priority' = view priority task
      title' = view title task
      tags' = view tags task
