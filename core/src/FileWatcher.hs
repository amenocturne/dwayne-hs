{-# LANGUAGE OverloadedStrings #-}

module FileWatcher
  ( startWatcher,
    WatcherHandle,
    stopWatcher,
  )
where

import Control.Concurrent (forkIO, threadDelay)
import Control.Concurrent.STM (TVar, atomically, modifyTVar', newTVarIO, readTVar)
import Control.Monad (forever, void, when)
import qualified Data.Set as Set
import System.FSNotify
import System.FilePath (normalise, takeDirectory, takeFileName)

data WatcherHandle = WatcherHandle
  { watchManager :: WatchManager,
    watchStop :: StopListening
  }

startWatcher :: [FilePath] -> IO () -> IO WatcherHandle
startWatcher files callback = do
  mgr <- startManager

  recentChanges <- newTVarIO Set.empty

  void $
    forkIO $
      forever $ do
        threadDelay 100000
        atomically $ modifyTVar' recentChanges (const Set.empty)

  let watchFile file = do
        let dir = takeDirectory file
            fileName = takeFileName file
        watchDir
          mgr
          dir
          ( \event -> case event of
              Modified path _ _ -> normalise path == normalise file
              _ -> False
          )
          ( \event -> do
              let path = eventPath event
              alreadySeen <- atomically $ do
                seen <- readTVar recentChanges
                if Set.member path seen
                  then return True
                  else do
                    modifyTVar' recentChanges (Set.insert path)
                    return False
              when (not alreadySeen) callback
          )

  stops <- mapM watchFile files
  let combinedStop = sequence_ stops

  return $ WatcherHandle mgr combinedStop

stopWatcher :: WatcherHandle -> IO ()
stopWatcher (WatcherHandle mgr stop) = do
  stop
  stopManager mgr
