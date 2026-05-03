{-# LANGUAGE OverloadedStrings #-}

module Model.OrgFormat
  ( -- * Priority formatting
    formatPriority,

    -- * Tags formatting
    formatTags,

    -- * Time formatting
    formatOrgTime,
    formatTimeField,
    formatRepeatInterval,
    formatDelayInterval,
    formatCreatedProp,

    -- * Header formatting
    formatStars,
    formatHeaderLine,

    -- * RichText formatting
    formatRichTextPlain,
  )
where

import Control.Lens (view)
import Data.Char (ord)
import qualified Data.Set as S
import qualified Data.Text as T
import Data.Time (Day, LocalTime, defaultTimeLocale)
import Data.Time.Format (formatTime)
import GHC.Char (chr)
import Model.Injection
import Model.OrgMode

-- | Format priority as [#A], [#B], etc. Returns Nothing for invalid priorities
formatPriority :: Int -> Maybe T.Text
formatPriority p
  | p >= 0 && p <= ord 'Z' - ord 'A' = Just $ T.pack $ "[#" ++ [chr $ ord 'A' + p] ++ "]"
  | otherwise = Nothing

-- | Format tags as :tag1:tag2:tag3: or empty text if no tags
formatTags :: [T.Text] -> T.Text
formatTags [] = ""
formatTags ts = T.concat [":", T.intercalate ":" ts, ":"]

-- | Format the time portion of an OrgTime (either Day or LocalTime)
formatOrgTime :: Either Day LocalTime -> T.Text
formatOrgTime (Left day) = T.pack $ formatTime defaultTimeLocale orgDayFormat day
formatOrgTime (Right utcTime) = T.pack $ formatTime defaultTimeLocale orgDayTimeFormat utcTime

-- | Format a repeat interval like "+1w" or "++2m"
formatRepeatInterval :: RepeatInterval -> T.Text
formatRepeatInterval (RepeatInterval tt v tu) =
  T.concat
    [ to tt,
      T.pack $ show v,
      T.singleton (to tu)
    ]

-- | Format a delay interval like "-1d" or "--2w"
formatDelayInterval :: DelayInterval -> T.Text
formatDelayInterval (DelayInterval tt v tu) =
  T.concat
    [ to tt,
      T.pack $ show v,
      T.singleton (to tu)
    ]

-- | Format the CREATED property.
formatCreatedProp :: Maybe OrgTime -> Maybe (T.Text, T.Text)
formatCreatedProp Nothing = Nothing
formatCreatedProp (Just t) =
  let timeText =
        case time t of
          Left day -> T.pack $ formatTime defaultTimeLocale orgDayFormat day
          Right localT -> T.pack $ formatTime defaultTimeLocale orgDayTimeFormat localT
   in Just (orgCreatedProperty, T.concat ["[", timeText, "]"])

-- | Format a complete time field with name, delimiters, repeater, and delay
-- Examples:
--   SCHEDULED: <2024-01-15 Mon>
--   DEADLINE: <2024-01-20 Sat +1w>
--   CLOSED: [2024-01-10 Wed]
formatTimeField :: TimeField -> OrgTime -> T.Text
formatTimeField (TimeField n delim) (OrgTime t r d) =
  T.concat
    [ n,
      ": ",
      T.singleton (fst delims),
      formatOrgTime t,
      maybe "" (\ri -> " " <> formatRepeatInterval ri) r,
      maybe "" (\di -> " " <> formatDelayInterval di) d,
      T.singleton (snd delims)
    ]
  where
    delims :: (Char, Char)
    delims = to delim

-- | Format task level as stars (1 -> "*", 2 -> "**", etc.)
formatStars :: Int -> T.Text
formatStars = flip T.replicate "*"

-- | Format RichText as plain text (for org-mode files)
formatRichTextPlain :: RichText -> T.Text
formatRichTextPlain (RichText nodes) = T.concat $ Prelude.map formatNode nodes
  where
    formatNode (PlainText t) = t
    formatNode (OrgLink url Nothing) = url
    formatNode (OrgLink url (Just title)) = T.concat ["[[", url, "][", title, "]]"]

-- | Format a complete header line without colors
-- Example: "** TODO [#A] Task title :tag1:tag2:"
formatHeaderLine :: Int -> T.Text -> Maybe Int -> RichText -> S.Set T.Text -> T.Text
formatHeaderLine level todoKw maybePriority title tags =
  T.intercalate " " $ filter (not . T.null) parts
  where
    parts =
      [ formatStars level,
        todoKw,
        maybe "" id (formatPriority =<< maybePriority),
        formatRichTextPlain title,
        formatTags (S.toList tags)
      ]
