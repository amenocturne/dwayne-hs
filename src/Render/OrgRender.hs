{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}

module Render.OrgRender where

import Brick
import Data.Char (ord)
import Data.List (intercalate)
import qualified Data.Text as T
import Data.Time (defaultTimeLocale)
import Data.Time.Format (formatTime)
import GHC.Char (chr)
import Model.OrgMode
import Render.Render

justList :: [Maybe a] -> [a]
justList [] = []
justList (x : xs) = case x of
  Just v -> v : justList xs
  Nothing -> justList xs

instance RenderTask Task b where
  renderCompact task = titleLine
   where
    titleLine =
      str $
        unwords $
          justList
            [ Just $ replicate (level task) '*'
            , Just $ T.unpack (todoKeyword task) -- TODO: colorcode them
            , priority task >>= renderPriority
            , Just $ T.unpack $ title task -- TODO: should fold if it is too long
            , renderTags (tags task)
            ]

    renderPriority p
      | p >= 0 = Just $ concat ["[#", [chr $ ord 'A' + p], "]"]
      | otherwise = Nothing

    renderTags [] = Nothing
    renderTags ts = Just $ ":" ++ intercalate ":" (fmap T.unpack ts) ++ ":" -- TODO: fold if there are too many tags

  renderFull task =
    vBox $
      justList
        [ Just titleLine
        , Just timeFieldsLine
        , Just $ txt orgPropertiesBegin
        , Just propertiesSection
        , Just $ txt orgPropertiesEnd
        , Just $ txt "\n"
        , Just $ txt $ description task -- TODO: fix spacing from :END:
        ]
   where
    titleLine =
      strWrap $
        unwords $
          justList
            [ Just $ replicate (level task) '*'
            , Just $ T.unpack (todoKeyword task) -- TODO: colorcode them
            , priority task >>= renderPriority
            , Just $ T.unpack $ title task
            , renderTags (tags task)
            ]
    timeFieldsLine =
      str $
        unwords $
          justList $
            fmap
              (\(field, getTime) -> fmap (renderTimeField field) (getTime task))
              [ (orgScheduledField, scheduled)
              , (orgDeadlineField, deadline)
              , (orgClosedField, closed)
              ]

    renderTimeField (TimeField n d) =
      (\s -> T.unpack n ++ " " ++ [fst $ delims d] ++ s ++ [snd $ delims d]) . displayOrgTime

    propertiesSection = vBox (fmap (\(key, value) -> str (":" ++ T.unpack key ++ ": " ++ T.unpack value)) (properties task))

    renderPriority p
      | p >= 0 = Just $ concat ["[#", [chr $ ord 'A' + p], "]"]
      | otherwise = Nothing

    renderTags [] = Nothing
    renderTags ts = Just $ ":" ++ intercalate ":" (fmap T.unpack ts) ++ ":"

-- Format OrgTime for display
displayOrgTime :: OrgTime -> String
displayOrgTime (OrgTime (Left day) _ _) = formatTime defaultTimeLocale orgDayFormat day -- TODO:
displayOrgTime (OrgTime (Right utcTime) _ _) = formatTime defaultTimeLocale orgDayTimeFormat utcTime -- TODO:
