{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Commands.OpenUrl (openUrlCommand, openTaskUrl, extractFirstUrl, openUrlInBrowser) where

import Brick (get)
import Brick.BChan (writeBChan)
import Commands.Command (Command (..), TuiBinding (..))
import Control.Applicative ((<|>))
import Control.Exception (IOException, catch)
import Control.Lens
import Control.Monad.IO.Class (liftIO)
import qualified Data.ByteString.Char8 as B
import Data.List.NonEmpty (NonEmpty (..))
import qualified Data.Set as Set
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Graphics.Vty.Input.Events as E
import Model.OrgMode (Task (..))
import System.Process (spawnProcess, waitForProcess)
import Text.Regex.Posix ((=~))
import qualified Tui.Contexts as Ctx
import Tui.Keybindings
import Tui.Types
  ( AppEvent (..),
    AppMode (NormalMode),
    GlobalAppState,
    KeyEvent (..),
    KeyPress (..),
    appState,
    currentTaskLens,
    eventChannel,
  )

-- | Open URL in task command
openUrlCommand :: Command Task
openUrlCommand =
  Command
    { cmdName = "Open URL",
      cmdAlias = "openUrl",
      cmdDescription = "Open the first URL found in the current task (title or description) in the default browser",
      cmdTui =
        Just $
          TuiBinding
            { tuiKeyEvent = OpenUrl,
              tuiKeybinding = toKeySeq "gx", -- 'g' then 'x'
              tuiDescription = "Open URL in task",
              tuiAction = openTaskUrl,
              tuiContext = Ctx.modeKeyContext NormalMode
            },
      cmdCli = Nothing,
      cmdApi = Nothing
    }

-- | Action: Open first URL found in current task
openTaskUrl :: GlobalAppState Task
openTaskUrl = do
  ctx <- get
  let ct = preview currentTaskLens ctx
  case ct of
    Just task -> do
      let titleUrl = extractFirstUrl (_title task)
          descUrl = extractFirstUrl (_description task)
          firstUrl = titleUrl <|> descUrl
      case firstUrl of
        Just url -> do
          result <- liftIO $ openUrlInBrowser url
          case result of
            Left err -> liftIO $ writeBChan (view (appState . eventChannel) ctx) $ Error err
            Right () -> return ()
        Nothing -> return ()
    Nothing -> return ()

-- | Extract the first URL from text (supports org-mode [[url]] format and plain URLs)
extractFirstUrl :: T.Text -> Maybe T.Text
extractFirstUrl text =
  let bs = TE.encodeUtf8 text
      orgPattern, urlPattern :: B.ByteString
      orgPattern = "\\[\\[(https?://[^ \t\n\\]]+)\\]\\]"
      urlPattern = "(https?://[^ \t\n]+)"
      orgMatches :: [[B.ByteString]] = bs =~ orgPattern
      urlMatches :: [[B.ByteString]] = bs =~ urlPattern
   in case orgMatches of
        ((_ : url : _) : _) -> Just $ TE.decodeUtf8 url
        _ -> case urlMatches of
          ((url : _) : _) -> Just $ TE.decodeUtf8 url
          _ -> Nothing

-- | Open a URL in the default browser using the 'open' command
openUrlInBrowser :: T.Text -> IO (Either String ())
openUrlInBrowser url = catch tryOpen handleError
  where
    tryOpen = do
      -- Use spawnProcess to avoid shell injection - passes URL as argument directly
      ph <- spawnProcess "open" [T.unpack url]
      _ <- waitForProcess ph
      return $ Right ()

    handleError :: IOException -> IO (Either String ())
    handleError e =
      return $
        Left $
          unlines
            [ "Failed to open URL in browser",
              "URL: " ++ T.unpack url,
              "Reason: " ++ show e
            ]
