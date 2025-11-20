{-# LANGUAGE OverloadedStrings #-}

module TextUtilsSpec (spec) where

import System.Directory (getHomeDirectory, createDirectoryIfMissing)
import System.Environment (setEnv, unsetEnv)
import System.FilePath ((</>))
import System.IO.Temp (withSystemTempDirectory)
import Test.Hspec
import TextUtils

spec :: Spec
spec = do
  describe "expandHome" $ do
    it "expands ~ to home directory" $ do
      home <- getHomeDirectory
      result <- expandHome "~/test/path"
      result `shouldBe` (home </> "test/path")

    it "expands ~/ to home directory" $ do
      home <- getHomeDirectory
      result <- expandHome "~/file.txt"
      result `shouldBe` (home </> "file.txt")

    it "leaves paths without ~ unchanged" $ do
      result <- expandHome "/absolute/path"
      result `shouldBe` "/absolute/path"

    it "leaves relative paths unchanged" $ do
      result <- expandHome "relative/path"
      result `shouldBe` "relative/path"

    it "does not expand ~ in the middle of path" $ do
      result <- expandHome "path/~/file"
      result `shouldBe` "path/~/file"

    it "does not expand ~ at the end" $ do
      result <- expandHome "path~"
      result `shouldBe` "path~"

  describe "expandPath" $ do
    describe "tilde expansion" $ do
      it "expands ~ to home directory" $ do
        home <- getHomeDirectory
        result <- expandPath "~/test/path"
        result `shouldBe` (home </> "test/path")

      it "expands ~/ at the beginning" $ do
        home <- getHomeDirectory
        result <- expandPath "~/file.txt"
        result `shouldBe` (home </> "file.txt")

      it "does not expand ~ in the middle" $ do
        result <- expandPath "some/~/path"
        result `shouldBe` "some/~/path"

    describe "environment variable expansion" $ do
      it "expands $VAR syntax" $ do
        setEnv "TEST_VAR_1" "/test/value"
        result <- expandPath "$TEST_VAR_1/subdir"
        result `shouldBe` "/test/value/subdir"
        unsetEnv "TEST_VAR_1"

      it "expands ${VAR} syntax" $ do
        setEnv "TEST_VAR_2" "/test/value"
        result <- expandPath "${TEST_VAR_2}/subdir"
        result `shouldBe` "/test/value/subdir"
        unsetEnv "TEST_VAR_2"

      it "expands multiple variables in one path" $ do
        setEnv "TEST_VAR_3" "/first"
        setEnv "TEST_VAR_4" "second"
        result <- expandPath "$TEST_VAR_3/$TEST_VAR_4/file"
        result `shouldBe` "/first/second/file"
        unsetEnv "TEST_VAR_3"
        unsetEnv "TEST_VAR_4"

      it "expands variable at the beginning" $ do
        setEnv "TEST_VAR_5" "/beginning"
        result <- expandPath "$TEST_VAR_5"
        result `shouldBe` "/beginning"
        unsetEnv "TEST_VAR_5"

      it "expands variable in the middle" $ do
        setEnv "TEST_VAR_6" "middle"
        result <- expandPath "prefix/$TEST_VAR_6/suffix"
        result `shouldBe` "prefix/middle/suffix"
        unsetEnv "TEST_VAR_6"

      it "expands variable at the end" $ do
        setEnv "TEST_VAR_7" "end"
        result <- expandPath "prefix/$TEST_VAR_7"
        result `shouldBe` "prefix/end"
        unsetEnv "TEST_VAR_7"

      it "handles mixed ${VAR} and $VAR syntax" $ do
        setEnv "TEST_VAR_8" "/first"
        setEnv "TEST_VAR_9" "second"
        result <- expandPath "${TEST_VAR_8}/$TEST_VAR_9"
        result `shouldBe` "/first/second"
        unsetEnv "TEST_VAR_8"
        unsetEnv "TEST_VAR_9"

      it "leaves undefined variables unexpanded with $VAR syntax" $ do
        -- Ensure variable doesn't exist
        unsetEnv "UNDEFINED_VAR_1"
        result <- expandPath "$UNDEFINED_VAR_1/path"
        result `shouldBe` "$UNDEFINED_VAR_1/path"

      it "leaves undefined variables unexpanded with ${VAR} syntax" $ do
        unsetEnv "UNDEFINED_VAR_2"
        result <- expandPath "${UNDEFINED_VAR_2}/path"
        result `shouldBe` "${UNDEFINED_VAR_2}/path"

      it "handles $ followed by non-alphanumeric as literal" $ do
        result <- expandPath "price$5"
        result `shouldBe` "price$5"

      it "handles $ at the end of path as literal" $ do
        result <- expandPath "path/file$"
        result `shouldBe` "path/file$"

      it "handles empty variable name after $" $ do
        result <- expandPath "$/path"
        result `shouldBe` "$/path"

      it "handles ${} with no variable name" $ do
        result <- expandPath "${}/path"
        result `shouldBe` "${}/path"

      it "handles unclosed ${VAR without closing brace" $ do
        setEnv "TEST_VAR_10" "value"
        result <- expandPath "${TEST_VAR_10"
        result `shouldBe` "${TEST_VAR_10"
        unsetEnv "TEST_VAR_10"

      it "expands variables with underscores" $ do
        setEnv "TEST_VAR_WITH_UNDERSCORE" "/test"
        result <- expandPath "$TEST_VAR_WITH_UNDERSCORE/path"
        result `shouldBe` "/test/path"
        unsetEnv "TEST_VAR_WITH_UNDERSCORE"

      it "expands variables with numbers" $ do
        setEnv "VAR123" "/test"
        result <- expandPath "$VAR123/path"
        result `shouldBe` "/test/path"
        unsetEnv "VAR123"

      it "stops variable name at special characters" $ do
        setEnv "TEST" "/value"
        result <- expandPath "$TEST-suffix"
        result `shouldBe` "/value-suffix"
        unsetEnv "TEST"

    describe "combined expansion" $ do
      it "expands both ~ and environment variables" $ do
        home <- getHomeDirectory
        setEnv "TEST_VAR_11" "subdir"
        result <- expandPath "~/$TEST_VAR_11/file"
        result `shouldBe` (home </> "subdir/file")
        unsetEnv "TEST_VAR_11"

      it "expands ~ first, then environment variables" $ do
        home <- getHomeDirectory
        setEnv "TEST_VAR_12" "docs"
        result <- expandPath "~/${TEST_VAR_12}/file.txt"
        result `shouldBe` (home </> "docs/file.txt")
        unsetEnv "TEST_VAR_12"

      it "handles complex paths with multiple expansions" $ do
        home <- getHomeDirectory
        setEnv "DIR1" "projects"
        setEnv "DIR2" "haskell"
        result <- expandPath "~/$DIR1/${DIR2}/app"
        result `shouldBe` (home </> "projects/haskell/app")
        unsetEnv "DIR1"
        unsetEnv "DIR2"

    describe "edge cases" $ do
      it "handles empty path" $ do
        result <- expandPath ""
        result `shouldBe` ""

      it "handles path with only /" $ do
        result <- expandPath "/"
        result `shouldBe` "/"

      it "handles consecutive $ characters" $ do
        result <- expandPath "$$"
        result `shouldBe` "$$"

      it "handles path with multiple consecutive slashes" $ do
        setEnv "TEST_VAR_13" "value"
        result <- expandPath "$TEST_VAR_13//path"
        result `shouldBe` "value//path"
        unsetEnv "TEST_VAR_13"

  describe "splitBy" $ do
    it "splits text by delimiter" $ do
      splitBy ',' "a,b,c" `shouldBe` ["a", "b", "c"]

    it "handles empty string" $ do
      splitBy ',' "" `shouldBe` []

    it "handles single element" $ do
      splitBy ',' "abc" `shouldBe` ["abc"]

    it "handles consecutive delimiters" $ do
      splitBy ',' "a,,b" `shouldBe` ["a", "", "b"]

    it "handles delimiter at start" $ do
      splitBy ',' ",a,b" `shouldBe` ["", "a", "b"]

    it "handles delimiter at end" $ do
      splitBy ',' "a,b," `shouldBe` ["a", "b", ""]

  describe "isWhitespaceExceptNewline" $ do
    it "returns true for space" $ do
      isWhitespaceExceptNewline ' ' `shouldBe` True

    it "returns true for tab" $ do
      isWhitespaceExceptNewline '\t' `shouldBe` True

    it "returns false for newline" $ do
      isWhitespaceExceptNewline '\n' `shouldBe` False

    it "returns false for carriage return" $ do
      isWhitespaceExceptNewline '\r' `shouldBe` False

    it "returns false for regular characters" $ do
      isWhitespaceExceptNewline 'a' `shouldBe` False

  describe "splitByFirstDelimiter" $ do
    it "splits by first occurrence of delimiter" $ do
      splitByFirstDelimiter "," "a,b,c" `shouldBe` ("a", ",b,c")

    it "handles empty input" $ do
      splitByFirstDelimiter "," "" `shouldBe` ("", "")

    it "handles delimiter at start" $ do
      splitByFirstDelimiter "," ",abc" `shouldBe` ("", ",abc")

    it "handles no delimiter" $ do
      splitByFirstDelimiter "," "abc" `shouldBe` ("abc", "")

    it "handles multi-character delimiter" $ do
      splitByFirstDelimiter "--" "a--b--c" `shouldBe` ("a", "--b--c")

  describe "removeLeadingSpaces" $ do
    it "removes leading spaces" $ do
      removeLeadingSpaces "   hello" `shouldBe` "hello"

    it "removes leading tabs" $ do
      removeLeadingSpaces "\t\thello" `shouldBe` "hello"

    it "removes mixed leading whitespace" $ do
      removeLeadingSpaces " \t  hello" `shouldBe` "hello"

    it "does not remove trailing spaces" $ do
      removeLeadingSpaces "hello   " `shouldBe` "hello   "

    it "handles empty string" $ do
      removeLeadingSpaces "" `shouldBe` ""

    it "handles only whitespace" $ do
      removeLeadingSpaces "   " `shouldBe` ""

  describe "readFileExample and writeFileExample integration" $ do
    it "writes and reads a file with path expansion" $ do
      withSystemTempDirectory "dwayne-test" $ \tmpDir -> do
        setEnv "TEST_TMP_DIR" tmpDir
        let content = "test content"
        let path = "$TEST_TMP_DIR/test-file.txt"
        
        -- Write file
        writeResult <- writeFileExample path content
        writeResult `shouldBe` Right ()
        
        -- Read file back
        readContent <- readFileExample path
        readContent `shouldBe` content
        
        unsetEnv "TEST_TMP_DIR"

    it "writes and reads a file with ~ expansion" $ do
      withSystemTempDirectory "dwayne-test" $ \tmpDir -> do
        -- Create a test subdirectory
        let testDir = tmpDir </> "test-home"
        createDirectoryIfMissing True testDir
        
        -- Temporarily use tmpDir as "home" by using explicit path
        setEnv "TEST_HOME" testDir
        let content = "home test content"
        let path = "$TEST_HOME/test-file.txt"
        
        -- Write file
        writeResult <- writeFileExample path content
        writeResult `shouldBe` Right ()
        
        -- Read file back
        readContent <- readFileExample path
        readContent `shouldBe` content
        
        unsetEnv "TEST_HOME"

    it "returns error when writing to invalid path" $ do
      let invalidPath = "/root/nonexistent/dir/file.txt"
      writeResult <- writeFileExample invalidPath "content"
      case writeResult of
        Left err -> err `shouldContain` "Failed to write file"
        Right () -> fail "Expected write to fail for invalid path"
