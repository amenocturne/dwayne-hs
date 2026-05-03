{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}

module Commands.RegistrySpec (spec) where

import Api.Types (ApiBinding (..))
import Commands.Command (Command (..), TuiBinding (..))
import Commands.Registry (allCommands)
import Data.List (group, isPrefixOf, sort)
import qualified Data.List.NonEmpty as NE
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

    it "has no keybinding prefix conflicts" $ do
      let bindings =
            mapMaybe
              (\cmd -> (cmdAlias cmd,) . NE.toList . tuiKeybinding <$> cmdTui cmd)
              (allCommands :: [Command Task])
          conflicts =
            [ (fst a, fst b)
            | a <- bindings,
              b <- bindings,
              snd a /= snd b,
              snd a `isPrefixOf` snd b
            ]
      conflicts `shouldBe` []

    it "has no duplicate API endpoints" $ do
      let endpoints =
            mapMaybe
              (\cmd -> (cmdAlias cmd,) . apiEndpoint <$> cmdApi cmd)
              (allCommands :: [Command Task])
          dupes =
            filter ((> 1) . length) . group . sort $
              map snd endpoints
      dupes `shouldBe` []

    it "has no CLI subcommand prefix conflicts" $ do
      let cliAliases =
            mapMaybe
              (\cmd -> case cmdCli cmd of Just _ -> Just (T.unpack (cmdAlias cmd)); Nothing -> Nothing)
              (allCommands :: [Command Task])
          conflicts =
            [ (a, b)
            | a <- cliAliases,
              b <- cliAliases,
              a /= b,
              a `isPrefixOf` b
            ]
      conflicts `shouldBe` []
