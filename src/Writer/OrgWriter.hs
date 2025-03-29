{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Writer.OrgWriter where

import Data.Char (ord)
import qualified Data.Text as T
import Data.Time (defaultTimeLocale)
import Data.Time.Format (formatTime)
import GHC.Char (chr)
import Model.OrgMode
import Writer.Writer

instance Writer a => Writer (TaskFile a) where
  write (TaskFile maybeName tasks) = T.intercalate "\n" $ titleText ++ tasksText
   where
    titleText = case maybeName of
      Just n -> ["#+TITLE: " <> n, ""]
      Nothing -> []
    tasksText = map write tasks

-- TODO: add writing of repeater and delay
-- TODO: refactor into smaller writers
instance Writer Task where
  write task = T.intercalate "\n" $ filter (not . T.null) components
   where
    desc = T.strip (description task)
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
          [ T.replicate (level task) "*"
          , todoKeyword task
          , renderPriorityText (priority task)
          , title task
          , renderTagsText (tags task)
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
      T.intercalate " " $
        filter
          (not . T.null)
          [ renderTimeFieldText orgScheduledField (scheduled task)
          , renderTimeFieldText orgDeadlineField (deadline task)
          , renderTimeFieldText orgClosedField (closed task)
          ]

    propertiesSection
      | null (properties task) = ""
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
        map (\(key, value) -> T.concat [":", key, ": ", value]) (properties task)

    renderTimeFieldText :: TimeField -> Maybe OrgTime -> T.Text
    renderTimeFieldText _ Nothing = ""
    renderTimeFieldText (TimeField n delim) (Just t) =
      T.concat
        [ n
        , ": "
        , T.singleton (fst $ delims delim)
        , T.pack (displayOrgTime t)
        , T.singleton (snd $ delims delim)
        ]

    displayOrgTime :: OrgTime -> String
    displayOrgTime (OrgTime (Left day) _ _) = formatTime defaultTimeLocale orgDayFormat day -- TODO:
    displayOrgTime (OrgTime (Right utcTime) _ _) = formatTime defaultTimeLocale orgDayTimeFormat utcTime -- TODO:
