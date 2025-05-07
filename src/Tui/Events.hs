module Tui.Events where

import Brick
import Control.Lens
import Data.Functor
import Graphics.Vty.Input.Events
import Tui.Types

import Brick.Keybindings as K
import Brick.Widgets.Dialog (dialog)
import Data.List

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
  let dispatchers = view keyEventDispatchers ctx
  let maybeDispatcher = find (\x -> view dispatcherPrecondition x ctx) dispatchers
  case maybeDispatcher of
    Nothing -> return ()
    Just d -> void $ K.handleKey (view dispatcher d) key []
handleEvent (AppEvent event) = handleAppEvent event
handleEvent _ = return ()
