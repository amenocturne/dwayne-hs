{-# LANGUAGE OverloadedStrings #-}

-- | Compute a delta 'Event' from (oldTask, newTask).
--
-- Each Task field is compared. If it changed, the event carries the new value
-- (Just _, possibly carrying a Nullable sentinel for cleared optional fields).
-- If it did not change, the event leaves that field as Nothing.
module Events.Diff
  ( diffTasks,
    diffTaskAsEvent,
    fullEvent,
  )
where

import Core.Nullable (Nullable (..))
import qualified Data.Set as S
import Data.Time (UTCTime)
import Events.Types (Event (..), emptyEvent, genesisEvent)
import Model.OrgMode (OrgTime, Task (..))

-- | Build a delta event covering only the fields that differ between
-- 'old' and 'new'. The event is anchored at (fp, idx, occurredAt).
diffTaskAsEvent :: FilePath -> Int -> UTCTime -> Task -> Task -> Event
diffTaskAsEvent fp idx now old new =
  let base = emptyEvent fp idx now
      withL = if _level old == _level new then base else base {evLevel = Just (_level new)}
      withK = if _todoKeyword old == _todoKeyword new then withL else withL {evTodoKeyword = Just (_todoKeyword new)}
      withP = if _priority old == _priority new then withK else withK {evPriority = Just (priorityVal (_priority new))}
      withT = if _title old == _title new then withP else withP {evTitle = Just (_title new)}
      withTags = if _tags old == _tags new then withT else withT {evTags = Just (S.toList (_tags new))}
      withSch = if _scheduled old == _scheduled new then withTags else withTags {evScheduled = Just (orgVal (_scheduled new))}
      withDl = if _deadline old == _deadline new then withSch else withSch {evDeadline = Just (orgVal (_deadline new))}
      withCr = if _createdProp old == _createdProp new then withDl else withDl {evCreated = Just (orgVal (_createdProp new))}
      withCl = if _closed old == _closed new then withCr else withCr {evClosed = Just (orgVal (_closed new))}
      withProps = if _properties old == _properties new then withCl else withCl {evProperties = Just (_properties new)}
      withDesc = if _description old == _description new then withProps else withProps {evDescription = Just (_description new)}
   in withDesc

-- | Convenience tuple-form diff for tests.
diffTasks :: Task -> Task -> [String]
diffTasks old new =
  concat
    [ ["level" | _level old /= _level new],
      ["todoKeyword" | _todoKeyword old /= _todoKeyword new],
      ["priority" | _priority old /= _priority new],
      ["title" | _title old /= _title new],
      ["tags" | _tags old /= _tags new],
      ["scheduled" | _scheduled old /= _scheduled new],
      ["deadline" | _deadline old /= _deadline new],
      ["createdProp" | _createdProp old /= _createdProp new],
      ["closed" | _closed old /= _closed new],
      ["properties" | _properties old /= _properties new],
      ["description" | _description old /= _description new]
    ]

-- | Re-export of 'genesisEvent' so callers in this module's neighborhood
-- only need to import Events.Diff.
fullEvent :: FilePath -> Int -> UTCTime -> Task -> Event
fullEvent = genesisEvent

-- ---------------------------------------------------------------------------
-- helpers
-- ---------------------------------------------------------------------------

priorityVal :: Maybe Int -> Int
priorityVal (Just n) = n
priorityVal Nothing = nullValue

orgVal :: Maybe OrgTime -> OrgTime
orgVal (Just o) = o
orgVal Nothing = nullValue
