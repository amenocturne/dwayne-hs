{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Writer.OrgWriter where

import Control.Lens (view)
import Data.Maybe
import qualified Data.Set as S
import qualified Data.Text as T
import qualified Data.Vector as V
import Model.Injection
import Model.OrgFormat (formatHeaderLine, formatRichTextPlain, formatTimeField)
import Model.OrgMode
import Writer.Writer

instance (Writer a) => Writer (TaskFile a) where
  write (TaskFile maybeName tasks) = T.concat $ titleText ++ [tasksText]
    where
      titleText = case maybeName of
        Just n -> ["#+TITLE: " <> n, "\n\n"]
        Nothing -> []
      tasksText = T.intercalate "\n\n" $ V.toList $ V.map write tasks

instance Writer Task where
  write task = T.intercalate "\n" $ filter (not . T.null) components
    where
      desc = T.strip $ formatRichTextPlain (view description task)
      components =
        [ headerLine,
          timeFieldsLine,
          propertiesSection,
          if T.null desc then "" else T.concat ["\n", desc]
        ]

      headerLine =
        formatHeaderLine
          (view level task)
          (view todoKeyword task)
          (view priority task)
          (view title task)
          (view tags task)

      timeFieldsLine =
        T.intercalate " "
          $ filter
            (not . T.null)
          $ catMaybes
            [ fmap (formatTimeField orgClosedField) (view closed task),
              fmap (formatTimeField orgScheduledField) (view scheduled task),
              fmap (formatTimeField orgDeadlineField) (view deadline task)
            ]

      propertiesSection
        | null (view properties task) = ""
        | otherwise =
            T.strip $
              T.unlines $
                filter
                  (not . T.null)
                  [ orgPropertiesBegin,
                    propertiesText,
                    orgPropertiesEnd
                  ]

      propertiesText =
        T.intercalate "\n" $
          map (uncurry renderProperty) (view properties task)

      renderProperty key value =
        if key == orgCreatedProperty
          then T.concat [":", key, ":  ", value]
          else T.concat [":", key, ": ", value]
