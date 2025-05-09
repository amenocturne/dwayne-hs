{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Writer.OrgWriter where

import Control.Lens (view)
import Data.Char (ord)
import Data.Maybe
import qualified Data.Text as T
import Data.Time (defaultTimeLocale)
import Data.Time.Format (formatTime)
import GHC.Char (chr)
import Model.Injection
import Model.OrgMode
import Writer.Writer

instance (Writer a) => Writer (TaskFile a) where
  write (TaskFile maybeName tasks) = T.intercalate "\n" $ titleText ++ tasksText
   where
    titleText = case maybeName of
      Just n -> ["#+TITLE: " <> n, ""]
      Nothing -> []
    tasksText = map write tasks

instance Writer Task where
  write task = T.intercalate "\n" $ filter (not . T.null) components
   where
    desc = T.strip (view description task)
    components =
      [ headerLine
      , timeFieldsLine
      , propertiesSection
      , if T.null desc then desc else T.concat [desc, "\n"]
      ]

    headerLine =
      T.intercalate " " $
        filter
          (not . T.null)
          [ T.replicate (view level task) "*"
          , view todoKeyword task
          , renderPriorityText (view priority task)
          , view title task
          , renderTagsText (view tags task)
          ]

    renderPriorityText :: Maybe Int -> T.Text
    renderPriorityText Nothing = ""
    renderPriorityText (Just p)
      | p >= 0 = T.pack $ "[#" ++ [chr $ ord 'A' + p] ++ "]"
      | otherwise = ""

    renderTagsText :: [T.Text] -> T.Text
    renderTagsText [] = ""
    renderTagsText ts = T.concat [":", T.intercalate ":" ts, ":"]

    timeFieldsLine =
      T.intercalate " "
        $ filter
          (not . T.null)
        $ catMaybes
          [ fmap (renderTimeFieldText orgClosedField) (view closed task)
          , fmap (renderTimeFieldText orgScheduledField) (view scheduled task)
          , fmap (renderTimeFieldText orgDeadlineField) (view deadline task)
          ]

    propertiesSection
      | null (view properties task) = ""
      | otherwise =
          T.unlines $
            filter
              (not . T.null)
              [ orgPropertiesBegin
              , propertiesText
              , orgPropertiesEnd
              ]

    propertiesText =
      T.intercalate "\n" $
        map (\(key, value) -> T.concat [":", key, ":  ", value]) (view properties task)

    renderTimeFieldText :: TimeField -> OrgTime -> T.Text
    renderTimeFieldText (TimeField n delim) (OrgTime t r d) =
      T.concat
        [ n
        , ": "
        , T.singleton (fst delims)
        , T.pack (displayOrgTime t)
        , T.pack $ maybe "" renderRepeater r
        , T.pack $ maybe "" renderDelay d
        , T.singleton (snd delims)
        ]
     where
      delims :: (Char, Char)
      delims = to delim

      displayOrgTime (Left day) = formatTime defaultTimeLocale orgDayFormat day
      displayOrgTime (Right utcTime) = formatTime defaultTimeLocale orgDayTimeFormat utcTime
      renderRepeater :: RepeatInterval -> String
      renderRepeater (RepeatInterval tt v tu) =
        concat
          [ " "
          , T.unpack $ to tt
          , show v
          , [to tu]
          ]
      renderDelay :: DelayInterval -> String
      renderDelay (DelayInterval tt v tu) =
        concat
          [ " "
          , T.unpack $ to tt
          , show v
          , [to tu]
          ]
