{-# LANGUAGE OverloadedStrings #-}

module Commands.Tags
  ( addMusicTagCommand,
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
import qualified Data.Set as S
import qualified Data.Text as T
import Model.OrgMode (Task)
import qualified Tui.Contexts as Ctx
import qualified Tui.Helpers as Helpers
import Tui.Keybindings (toKeySeq)
import qualified Tui.SelectionMode as SM
import Tui.Types (KeyEvent (..), KeyPress)

-- | Tag operation type
data TagOperation = AddTagOp | DeleteTagOp

-- | Generic builder for tag commands
tagCommand ::
  -- | Tag operation (add or delete)
  TagOperation ->
  -- | Tag name
  T.Text ->
  -- | Key sequence (e.g., "a,m" for add music)
  String ->
  -- | Command alias (e.g., "addTagMusic")
  T.Text ->
  Command Task
tagCommand operation tag keySeq alias =
  Command
    { cmdName = case operation of
        AddTagOp -> "Add Tag"
        DeleteTagOp -> "Delete Tag",
      cmdAlias = alias,
      cmdDescription = T.concat [prefix, tag, " tag to/from the current task or selection"],
      cmdTui =
        Just $
          TuiBinding
            { tuiKeyEvent = case operation of
                AddTagOp -> Tui.Types.AddTag tag
                DeleteTagOp -> Tui.Types.DeleteTag tag,
              tuiKeybinding = toKeySeq keySeq,
              tuiDescription = T.concat [prefix, tag, " tag"],
              tuiAction = Helpers.saveForUndo $ SM.smartApplyTagAction tagOp,
              tuiContext = Ctx.normalOrSelectionContext
            },
      cmdCli = Nothing,
      cmdApi = Nothing
    }
  where
    prefix = case operation of
      AddTagOp -> "Add "
      DeleteTagOp -> "Delete "
    tagOp = case operation of
      AddTagOp -> S.insert tag
      DeleteTagOp -> S.delete tag

-- | Convenience builder for add tag commands
addTagCommand ::
  -- | Tag name
  T.Text ->
  -- | Key sequence (e.g., "a,m")
  String ->
  -- | Command alias (e.g., "addTagMusic")
  T.Text ->
  Command Task
addTagCommand = tagCommand AddTagOp

-- | Convenience builder for delete tag commands
deleteTagCommand ::
  -- | Tag name
  T.Text ->
  -- | Key sequence (e.g., "d,m")
  String ->
  -- | Command alias (e.g., "deleteTagMusic")
  T.Text ->
  Command Task
deleteTagCommand = tagCommand DeleteTagOp

-- | Predefined tag commands
addMusicTagCommand :: Command Task
addMusicTagCommand = addTagCommand "music" "a,m" "addTagMusic"

deleteMusicTagCommand :: Command Task
deleteMusicTagCommand = deleteTagCommand "music" "d,m" "deleteTagMusic"

addCoolTagCommand :: Command Task
addCoolTagCommand = addTagCommand "cool" "a,c" "addTagCool"

deleteCoolTagCommand :: Command Task
deleteCoolTagCommand = deleteTagCommand "cool" "d,c" "deleteTagCool"

addSoftwareTagCommand :: Command Task
addSoftwareTagCommand = addTagCommand "software" "a,s" "addTagSoftware"

deleteSoftwareTagCommand :: Command Task
deleteSoftwareTagCommand = deleteTagCommand "software" "d,s" "deleteTagSoftware"

addBookTagCommand :: Command Task
addBookTagCommand = addTagCommand "book" "a,b" "addTagBook"

deleteBookTagCommand :: Command Task
deleteBookTagCommand = deleteTagCommand "book" "d,b" "deleteTagBook"
