{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TupleSections #-}

module Core.Filters
  ( computeFilteredSortedView,
  )
where

import Control.Lens
import Control.Monad.ST (runST)
import Core.Types
import qualified Data.Map.Strict as M
import qualified Data.Vector as V
import qualified Data.Vector.Algorithms.Intro as VA

-- | Compute a filtered and sorted view of tasks from a FileState.
-- Takes a list of filter predicates (AND logic), a sorting function, and the FileState.
-- Returns a vector of task-pointer pairs.
computeFilteredSortedView ::
  [a -> Bool] ->
  (a -> a -> Ordering) ->
  FileState a ->
  V.Vector (a, TaskPointer)
computeFilteredSortedView filters sorter fs =
  let allPtrs = getAllPointers fs
      ptrsWithTasks = V.mapMaybe (\ptr -> fmap (,ptr) (fs ^? taskBy ptr)) allPtrs
      filtered = V.filter (\(task, _) -> all (\f -> f task) filters) ptrsWithTasks
      sorted = sortByVector (\(t1, _) (t2, _) -> sorter t1 t2) filtered
   in sorted

-- | Get all task pointers from a FileState.
getAllPointers :: FileState a -> V.Vector TaskPointer
getAllPointers fs = V.concat $ M.foldlWithKey' accumulate [] fs
  where
    accumulate acc f result =
      case resultToMaybe result of
        Nothing -> acc
        Just taskFile ->
          let taskCount = V.length (_content taskFile)
              pointers = V.generate taskCount (\i -> TaskPointer f i)
           in pointers : acc

-- | Traversal to a specific task in the FileState.
taskBy :: TaskPointer -> Traversal' (FileState a) a
taskBy ptr =
  ix (view file ptr)
    . success
    . content
    . ix (view taskIndex ptr)

-- | Sort a vector using a comparison function (efficient ST-based sorting).
sortByVector :: (a -> a -> Ordering) -> V.Vector a -> V.Vector a
sortByVector cmp vec = runST $ do
  mvec <- V.thaw vec
  VA.sortBy cmp mvec
  V.freeze mvec

-- | Traversal for accessing ParserSuccess content.
success :: Traversal' (ParserResult a) a
success f (ParserSuccess x) = ParserSuccess <$> f x
success _ e@(ParserFailure _) = pure e
