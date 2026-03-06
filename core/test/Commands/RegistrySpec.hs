{-# LANGUAGE OverloadedStrings #-}

module Commands.RegistrySpec (spec) where

import Commands.Command (Command (..))
import Commands.Registry (allCommands)
import Data.List (group, sort)
import Data.Maybe (mapMaybe)
import qualified Data.Text as T
import Model.OrgMode (Task)
import Test.Hspec

spec :: Spec
spec = describe "Commands.Registry" $ do
  describe "allCommands" $ do
    it "has no duplicate command aliases" $ do
      let aliases = map (T.unpack . cmdAlias) allCommands
          dupes = filter ((> 1) . length) . group . sort $ aliases
      dupes `shouldBe` []

    it "has no duplicate CLI subcommand names" $ do
      let cliNames = mapMaybe (\cmd -> case cmdCli cmd of Just _ -> Just (T.unpack (cmdAlias cmd)); Nothing -> Nothing) (allCommands :: [Command Task])
          dupes = filter ((> 1) . length) . group . sort $ cliNames
      dupes `shouldBe` []
