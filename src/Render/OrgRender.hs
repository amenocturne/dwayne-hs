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
import Graphics.Vty.Attributes (defAttr, withURL)
import Model.Injection
import Model.OrgFormat
import Model.OrgMode
import Render.Render
import Tui.ColorScheme (ColorScheme, descriptionAttr, levelAttr, priorityAttr, propertyAttr, tagAttr, timeFieldAttr, todoKeywordAttr, urlAttr)

renderRichText :: RichText -> Widget n
renderRichText (RichText nodes) = hBox $ Prelude.map renderNode nodes
  where
    renderNode (PlainText t) = txt t
    renderNode (OrgLink url (Just title)) =
      withAttr urlAttr $ txt title
    renderNode (OrgLink url Nothing) =
      withAttr urlAttr $ txt url

instance Render Task b where
  renderCompact scheme task =
    hBox $
      catMaybes
        [ Just $ withAttr (levelAttr level') $ txt $ formatStars level',
          Just $ txt " ",
          Just $ withAttr (todoKeywordAttr todoKw) $ txt todoKw,
          Just $ txt " ",
          case priority' of
            Just p -> fmap (\t -> withAttr (priorityAttr p) $ txt $ t <> " ") (formatPriority p)
            Nothing -> Nothing,
          Just $ renderRichText title',
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

  -- TODO: make title wrap in full view
  renderFull scheme task =
    vBox $
      catMaybes
        [ Just titleLine,
          Just timeFieldsLine,
          Just $ withAttr propertyAttr $ txt orgPropertiesBegin,
          Just propertiesSection,
          Just $ withAttr propertyAttr $ txt orgPropertiesEnd,
          Just $ txt "\n",
          Just $ withAttr descriptionAttr $ renderRichText (view description task)
        ]
    where
      titleLine =
        hBox $
          catMaybes
            [ Just $ withAttr (levelAttr level') $ txt $ formatStars level',
              Just $ txt " ",
              Just $ withAttr (todoKeywordAttr todoKw) $ txt todoKw,
              Just $ txt " ",
              case priority' of
                Just p -> fmap (\t -> withAttr (priorityAttr p) $ txt $ t <> " ") (formatPriority p)
                Nothing -> Nothing,
              Just $ renderRichText title',
              case formatTags (S.toList tags') of
                "" -> Nothing
                t -> Just $ withAttr tagAttr $ txt $ " " <> t
            ]

      timeFieldsLine =
        withAttr timeFieldAttr $
          txt $
            T.unwords $
              mapMaybe
                (\(field, getTime) -> fmap (formatTimeField field) (getTime task))
                [ (orgScheduledField, view scheduled),
                  (orgDeadlineField, view deadline),
                  (orgClosedField, view closed)
                ]

      propertiesSection =
        withAttr propertyAttr $
          txt $
            T.intercalate "\n" (fmap (\(key, value) -> T.concat [":", key, ": ", value]) (view properties task))

      level' = view level task
      todoKw = view todoKeyword task
      priority' = view priority task
      title' = view title task
      tags' = view tags task
