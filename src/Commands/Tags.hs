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

import Commands.Builders (mkAddTagCommand, mkDeleteTagCommand)
import Model.OrgMode (Task)
import Commands.Command (Command)

-- | Predefined tag commands
addMusicTagCommand :: Command Task
addMusicTagCommand = mkAddTagCommand "music" "a,m" "addTagMusic"

deleteMusicTagCommand :: Command Task
deleteMusicTagCommand = mkDeleteTagCommand "music" "d,m" "deleteTagMusic"

addCoolTagCommand :: Command Task
addCoolTagCommand = mkAddTagCommand "cool" "a,c" "addTagCool"

deleteCoolTagCommand :: Command Task
deleteCoolTagCommand = mkDeleteTagCommand "cool" "d,c" "deleteTagCool"

addSoftwareTagCommand :: Command Task
addSoftwareTagCommand = mkAddTagCommand "software" "a,s" "addTagSoftware"

deleteSoftwareTagCommand :: Command Task
deleteSoftwareTagCommand = mkDeleteTagCommand "software" "d,s" "deleteTagSoftware"

addBookTagCommand :: Command Task
addBookTagCommand = mkAddTagCommand "book" "a,b" "addTagBook"

deleteBookTagCommand :: Command Task
deleteBookTagCommand = mkDeleteTagCommand "book" "d,b" "deleteTagBook"
