{-# LANGUAGE TemplateHaskell #-}

module Model.LinearHistory where

import Control.Lens

data LinearHistory a = LinearHistory
  { _pastStates :: [a]
  , _currentState :: a
  , _futureStates :: [a]
  }

makeLenses ''LinearHistory

undo :: LinearHistory a -> LinearHistory a
undo h@(LinearHistory p c f) = case p of
  [] -> h
  x : xs -> LinearHistory xs x (c : f)

redo :: LinearHistory a -> LinearHistory a
redo h@(LinearHistory p c f) = case f of
  [] -> h
  x : xs -> LinearHistory (c : p) x xs

append :: a -> LinearHistory a -> LinearHistory a
append a (LinearHistory p c f) = LinearHistory (c : p) a []

initLinearHistory :: a -> LinearHistory a
initLinearHistory a = LinearHistory [] a []
