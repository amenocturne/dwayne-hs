{-# LANGUAGE OverloadedStrings #-}

module Commands.Sync
  ( syncCommand,
  )
where

import Commands.CliHelpers (loadConfig)
import Commands.Command (Command (..))
import Model.OrgMode (Task)
import Options.Applicative
  ( auto,
    help,
    long,
    metavar,
    option,
    short,
    showDefault,
    value,
  )
import Sync.Client (SyncOptions (..), defaultSyncOptions, runSyncDaemon)
import Sync.Config (loadCredentials)
import Tui.Types (AppConfig (..), getAllFiles)

data SyncCliOpts = SyncCliOpts
  { sciInterval :: Int,
    sciPullWindowHours :: Int
  }

syncCommand :: Command Task
syncCommand =
  Command
    { cmdName = "Sync",
      cmdAlias = "sync",
      cmdDescription = "Run the event-sourced sync daemon (poll remote, push local events)",
      cmdTui = Nothing,
      cmdCli =
        Just $
          fmap runSync $
            SyncCliOpts
              <$> option
                auto
                ( long "interval"
                    <> short 'i'
                    <> metavar "SECONDS"
                    <> value 5
                    <> showDefault
                    <> help "Poll interval in seconds"
                )
              <*> option
                auto
                ( long "pull-window-hours"
                    <> metavar "HOURS"
                    <> value 24
                    <> showDefault
                    <> help "How many hours back the pull asks the server to replay"
                ),
      cmdApi = Nothing
    }
  where
    runSync :: SyncCliOpts -> IO ()
    runSync (SyncCliOpts interval window) = do
      conf <- loadConfig
      creds <- loadCredentials
      let opts =
            defaultSyncOptions
              { soPullWindowHours = window,
                soDefaultOwnedFiles = getAllFiles conf
              }
      runSyncDaemon (_database conf) creds interval opts
