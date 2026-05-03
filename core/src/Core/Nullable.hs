{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Sentinel-based "null" representation for event-sourced fields.
--
-- The events table stores partial Task updates. A row's column is either
-- NULL ("this field was not touched by this event") or holds a typed value.
-- To distinguish "field cleared" from "field set" without nesting Maybes
-- everywhere, we use a per-type sentinel ('nullValue') for the cleared case.
--
-- Convention:
--   * Maybe-Just nullValue = field cleared by the event
--   * Maybe-Just other     = field set to that value
--   * Maybe-Nothing        = field untouched (NULL in the SQL row)
module Core.Nullable
  ( Nullable (..),
  )
where

import qualified Data.Set as S
import qualified Data.Text as T
import Data.Time
  ( Day,
    LocalTime (..),
    TimeOfDay (..),
    fromGregorian,
  )
import Model.OrgMode (OrgTime (..), RichText (..))

-- | Types that have a designated "null" value used as the cleared-field
-- sentinel in event rows.
class Nullable a where
  nullValue :: a
  isNull :: a -> Bool
  default isNull :: (Eq a) => a -> Bool
  isNull = (== nullValue)

instance Nullable Int where
  nullValue = -1

instance Nullable T.Text where
  nullValue = ""

instance Nullable [a] where
  nullValue = []
  isNull [] = True
  isNull _ = False

instance (Ord a) => Nullable (S.Set a) where
  nullValue = S.empty
  isNull = S.null

-- | Sentinel epoch used for cleared org times: 1970-01-01.
instance Nullable OrgTime where
  nullValue = OrgTime (Left epochDay) Nothing Nothing
    where
      epochDay :: Day
      epochDay = fromGregorian 1970 1 1
  isNull (OrgTime t _ _) =
    case t of
      Left d -> d == fromGregorian 1970 1 1
      Right (LocalTime d (TimeOfDay 0 0 0)) -> d == fromGregorian 1970 1 1
      _ -> False

instance Nullable RichText where
  nullValue = RichText []
  isNull (RichText nodes) = null nodes
