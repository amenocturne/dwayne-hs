{-# LANGUAGE OverloadedStrings #-}

module Commands.Tags
  ( tagCommand,
    addMusicTagCommand,
    deleteMusicTagCommand,
    addCoolTagCommand,
    deleteCoolTagCommand,
    addSoftwareTagCommand,
    deleteSoftwareTagCommand,
    addBookTagCommand,
    deleteBookTagCommand,
  )
where

import Commands.Command (Command (..), TuiBinding (..))
import Control.Lens
import Data.List.NonEmpty (NonEmpty (..))
import qualified Data.Set as S
import qualified Data.Set as Set
import qualified Data.Text as T
import qualified Graphics.Vty.Input.Events as E
import Model.OrgMode (Task)
import qualified Tui.Contexts as Ctx
import qualified Tui.Helpers as Helpers
import Tui.Keybindings
import qualified Tui.SelectionMode as SM
import Tui.Types
  ( AppContext,
    GlobalAppState,
    KeyEvent (..),
    KeyPress (..),
  )

-- | Generic function to create a tag command
tagCommand :: Bool -> T.Text -> String -> T.Text -> Command Task
tagCommand isAdd tag keySeq alias =
  Command
    { cmdName = if isAdd then "Add Tag" else "Delete Tag",
      cmdAlias = alias,
      cmdDescription = T.concat [if isAdd then "Add " else "Delete ", tag, " tag to/from the current task or selection"],
      cmdTui =
        Just $
          TuiBinding
            { tuiKeyEvent = if isAdd then AddTag tag else DeleteTag tag,
              tuiKeybinding = toKeySeq keySeq,
              tuiDescription = T.concat [if isAdd then "Add " else "Delete ", tag, " tag"],
              tuiAction = Helpers.saveForUndo $ SM.smartApplyTagAction (if isAdd then S.insert tag else S.delete tag),
              tuiContext = Ctx.normalOrSelectionContext
            },
      cmdCli = Nothing,
      cmdApi = Nothing
    }

-- | Predefined tag commands
addMusicTagCommand :: Command Task
addMusicTagCommand = tagCommand True "music" "a,m" "addTagMusic"

deleteMusicTagCommand :: Command Task
deleteMusicTagCommand = tagCommand False "music" "d,m" "deleteTagMusic"

addCoolTagCommand :: Command Task
addCoolTagCommand = tagCommand True "cool" "a,c" "addTagCool"

deleteCoolTagCommand :: Command Task
deleteCoolTagCommand = tagCommand False "cool" "d,c" "deleteTagCool"

addSoftwareTagCommand :: Command Task
addSoftwareTagCommand = tagCommand True "software" "a,s" "addTagSoftware"

deleteSoftwareTagCommand :: Command Task
deleteSoftwareTagCommand = tagCommand False "software" "d,s" "deleteTagSoftware"

addBookTagCommand :: Command Task
addBookTagCommand = tagCommand True "book" "a,b" "addTagBook"

deleteBookTagCommand :: Command Task
deleteBookTagCommand = tagCommand False "book" "d,b" "deleteTagBook"
