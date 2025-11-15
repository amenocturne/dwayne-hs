{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE OverloadedStrings #-}
{-# HLINT ignore "Use tuple-section" #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}

module TextUtils where

import Control.Exception (IOException, catch)
import Data.Char (isSpace)
import Data.Kind
import Data.Maybe
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Data.Time (ParseTime, defaultTimeLocale)
import Data.Time.Format (parseTimeM)
import GHC.IO.Exception (ExitCode (..))
import System.Directory (getHomeDirectory, removeFile)
import System.Environment (lookupEnv)
import System.Exit (die)
import System.FilePath ((</>))
import System.IO
import System.Process

getConfigPath :: IO FilePath
getConfigPath = do
  mConfigFile <- lookupEnv "DWAYNE_CONFIG"
  mXdg <- lookupEnv "XDG_CONFIG_HOME"
  home <- getHomeDirectory
  let dwayneConfig = "dwayne" </> "config.yml"
  return $ case (mConfigFile, mXdg) of
    (Just configFile, _) -> configFile
    (_, Just xdg) -> xdg </> dwayneConfig
    _ -> home </> ".config" </> dwayneConfig

expandHome :: String -> IO String
expandHome ('~' : '/' : rest) = do
  home <- getHomeDirectory
  return $ home </> rest
expandHome path = return path

-- TODO: expand env variables as well as ~
readFileExample :: FilePath -> IO T.Text
readFileExample f = do
  fp <- expandHome f
  TIO.readFile fp `catch` handleIOError fp
  where
    handleIOError :: FilePath -> IOException -> IO T.Text
    handleIOError expandedPath e =
      die $
        unlines
          [ "ERROR: Failed to read file",
            "File: " ++ f,
            "Expanded path: " ++ expandedPath,
            "",
            "Reason: " ++ show e,
            "",
            "Please check that:",
            "  - The file exists",
            "  - You have read permissions",
            "  - The path is correct"
          ]

-- TODO: expand env variables as well as ~
writeFileExample :: FilePath -> T.Text -> IO (Either String ())
writeFileExample path content = do
  (TIO.writeFile path content >> return (Right ())) `catch` handleIOError
  where
    handleIOError :: IOException -> IO (Either String ())
    handleIOError e =
      return $
        Left $
          unlines
            [ "Failed to write file: " ++ path,
              "Reason: " ++ show e
            ]

printExample :: T.Text -> IO ()
printExample = TIO.putStrLn

splitBy :: Char -> T.Text -> [T.Text]
splitBy _ "" = []
splitBy delimiterChar inputString = T.foldr f [T.pack ""] inputString
  where
    f :: Char -> [T.Text] -> [T.Text]
    f _ [] = []
    f currentChar allStrings@(partialString : handledStrings)
      | currentChar == delimiterChar = "" : allStrings
      | otherwise = T.cons currentChar partialString : handledStrings

isWhitespaceExceptNewline :: Char -> Bool
isWhitespaceExceptNewline c = isSpace c && c /= '\n' && c /= '\r'

splitByFirstDelimiter :: T.Text -> T.Text -> (T.Text, T.Text)
splitByFirstDelimiter _ "" = ("", "")
splitByFirstDelimiter delim input
  | delim `T.isPrefixOf` input = ("", input)
  | otherwise =
      case T.uncons input of
        Nothing -> ("", "") -- This case is already handled above, but being explicit
        Just (c, rest) ->
          let (prefix, remainder) = splitByFirstDelimiter delim rest
           in (T.cons c prefix, remainder)

split :: Char -> T.Text -> [T.Text]
split _ "" = []
split delim str =
  let (prefix, suffix) = T.break (== delim) str
   in prefix : case T.uncons suffix of
        Nothing -> []
        Just (_, rest) -> case split delim rest of
          [] -> [":"]
          x : xs -> T.cons delim x : xs

removeLeadingSpaces :: T.Text -> T.Text
removeLeadingSpaces = T.dropWhile isSpace

parseTimeWith :: forall (m :: Type -> Type) t. (MonadFail m, ParseTime t) => String -> T.Text -> m t
parseTimeWith format str = parseTimeM True defaultTimeLocale format (T.unpack str)

editWithEditor :: T.Text -> IO (Either String (Maybe T.Text))
editWithEditor content = catch tryEdit handleError
  where
    tryEdit = do
      editor <- fmap (fromMaybe "vim") (lookupEnv "EDITOR")
      (tempPath, tempHandle) <- openTempFile "/tmp" "edit.txt"
      hPutStr tempHandle (T.unpack content)
      hFlush tempHandle
      hClose tempHandle
      exitCode <- system (editor ++ " " ++ tempPath)
      case exitCode of
        ExitSuccess -> do
          newContent <- readFile tempPath >>= \c -> length c `seq` return c
          removeFile tempPath
          return $ Right (Just $ T.pack newContent)
        _ -> do
          removeFile tempPath
          return $ Right Nothing -- User cancelled
    handleError :: IOException -> IO (Either String (Maybe T.Text))
    handleError e =
      return $
        Left $
          unlines
            [ "Failed to open editor",
              "Reason: " ++ show e,
              "",
              "Please check that:",
              "  - Your EDITOR environment variable is set correctly",
              "  - The /tmp directory exists and is writable",
              "  - You have permissions to create temporary files"
            ]
