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

renderRichTextWrapped :: RichText -> Widget n
renderRichTextWrapped richText = renderRichTextWrappedWithPrefix [] 0 richText

renderRichTextWrappedWithPrefix :: [Widget n] -> Int -> RichText -> Widget n
renderRichTextWrappedWithPrefix prefix prefixWidth (RichText nodes) =
  Widget Greedy Fixed $ do
    c <- getContext
    let w = availWidth c
    render $ vBox $ wrapRichText w prefix prefixWidth nodes
  where
    wrapRichText width prefixWidgets prefixLen nodes' =
      let segments = concatMap nodeToSegments nodes'
       in buildLines width prefixWidgets prefixLen segments

    nodeToSegments (PlainText t) = map (\word -> (word, Nothing)) (T.words t)
    nodeToSegments (OrgLink url (Just title)) = [(title, Just urlAttr)]
    nodeToSegments (OrgLink url Nothing) = [(url, Just urlAttr)]

    buildLines width prefixWidgets prefixLen segments =
      case segments of
        [] -> if null prefixWidgets then [] else [hBox prefixWidgets]
        _ ->
          let (line, rest) = takeLine (width - prefixLen) segments
              firstLine = hBox $ prefixWidgets ++ [txt " "] ++ intersperse (txt " ") (map renderSegment line)
           in if null rest
                then [firstLine]
                else firstLine : buildLinesNoPrefix width rest

    buildLinesNoPrefix width segments =
      case segments of
        [] -> []
        _ ->
          let (line, rest) = takeLine width segments
           in renderSegments line : buildLinesNoPrefix width rest

    takeLine width segs =
      let go acc accWidth [] = (reverse acc, [])
          go acc accWidth (s@(text, _) : rest) =
            let len = T.length text
                needed = if null acc then len else len + 1
             in if accWidth + needed <= width
                  then go (s : acc) (accWidth + needed) rest
                  else
                    if null acc
                      then ([s], rest)
                      else (reverse acc, s : rest)
       in go [] 0 segs

    renderSegments segs =
      hBox $ intersperse (txt " ") $ map renderSegment segs

    renderSegment (text, Nothing) = txt text
    renderSegment (text, Just attr) = withAttr attr $ txt text

    intersperse _ [] = []
    intersperse _ [x] = [x]
    intersperse sep (x : xs) = x : sep : intersperse sep xs

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

  renderFull scheme task =
    vBox $
      catMaybes
        [ Just titleSection,
          Just timeFieldsLine,
          Just $ withAttr propertyAttr $ txt orgPropertiesBegin,
          Just propertiesSection,
          Just $ withAttr propertyAttr $ txt orgPropertiesEnd,
          Just $ txt "\n",
          Just $ withAttr descriptionAttr $ renderRichText (view description task)
        ]
    where
      titleSection =
        let prefixParts =
              [ formatStars level',
                " ",
                todoKw,
                " ",
                case priority' of
                  Just p -> fromMaybe "" (formatPriority p) <> " "
                  Nothing -> ""
              ]
            prefixText = T.concat prefixParts
            prefixWidth = T.length prefixText
            prefixWidgets =
              catMaybes
                [ Just $ withAttr (levelAttr level') $ txt $ formatStars level',
                  Just $ txt " ",
                  Just $ withAttr (todoKeywordAttr todoKw) $ txt todoKw,
                  Just $ txt " ",
                  case priority' of
                    Just p -> fmap (\t -> withAttr (priorityAttr p) $ txt $ t <> " ") (formatPriority p)
                    Nothing -> Nothing
                ]
            tagsWidget = case formatTags (S.toList tags') of
              "" -> []
              t -> [withAttr tagAttr $ txt $ " " <> t]
         in hBox
              [ renderRichTextWrappedWithPrefix prefixWidgets prefixWidth title',
                hBox tagsWidget
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

      allProperties = catMaybes [formatCreatedProp (view createdProp task)] ++ view properties task

      propertiesSection =
        withAttr propertyAttr $
          txt $
            T.intercalate "\n" (fmap renderProperty allProperties)

      renderProperty (key, value) =
        if key == orgCreatedProperty
          then T.concat [":", key, ":  ", value]
          else T.concat [":", key, ": ", value]

      level' = view level task
      todoKw = view todoKeyword task
      priority' = view priority task
      title' = view title task
      tags' = view tags task
