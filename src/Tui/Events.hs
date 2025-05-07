module Tui.Events where

import Brick
import Control.Lens
import Data.Functor
import Graphics.Vty.Input.Events
import Tui.Types

import Brick.Keybindings as K
import Brick.Widgets.Dialog (dialog)

handleAppEvent :: AppEvent -> GlobalAppState a
handleAppEvent event = case event of
  Error msg -> do
    let dlg =
          ErrorDialog
            { _edDialog =
                dialog
                  (Just $ str "Error")
                  (Just (Viewport1, [("OK", Viewport1, ())]))
                  50
            , _edMessage = msg
            }
    modify $ set (appState . errorDialog) (Just dlg)

handleEvent :: BrickEvent Name AppEvent -> GlobalAppState a
handleEvent (VtyEvent (EvKey key _)) = do
  ctx <- get
  case ctx ^. appState . errorDialog of
    Just _ ->
      -- Error dialog is open: handle dialog-specific keys
      case key of
        -- TODO: factor this out to key bindings as well
        KEnter -> modify $ over (appState . errorDialog) (const Nothing)
        KEsc -> modify $ over (appState . errorDialog) (const Nothing)
        _ -> return ()
    Nothing ->
      -- No dialog: dispatch as normal
      void $ K.handleKey (view keyEventDispatcher ctx) key []
handleEvent (AppEvent event) = handleAppEvent event
handleEvent _ = return ()
