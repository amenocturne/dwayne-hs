{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE OverloadedStrings #-}
{-# HLINT ignore "Use tuple-section" #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}

{-# HLINT ignore "Use ==" #-}

module Parser.Parser where

import Control.Lens (makeLenses)
import qualified Data.Text as T
import GHC.Base

------------------------------- PARSER ---------------------------------------

data Location = Location {line :: Int, column :: Int} deriving (Show, Eq)

type ParserInput = (Location -> Location, T.Text)
type ParserError = String
data ParserResult a = ParserSuccess {_success :: a} | ParserFailure {_error :: ParserError} deriving (Functor, Show, Eq)
newtype Parser a = Parser (ParserInput -> (ParserInput, ParserResult a)) deriving (Functor)

makeLenses ''ParserResult

instance Foldable ParserResult where
  foldMap _ (ParserFailure _) = mempty
  foldMap f (ParserSuccess x) = f x

instance Applicative ParserResult where
  pure = ParserSuccess
  (ParserSuccess f) <*> (ParserSuccess a) = ParserSuccess (f a)
  (ParserFailure e) <*> _ = ParserFailure e
  _ <*> (ParserFailure e) = ParserFailure e

instance Traversable ParserResult where
  traverse _ (ParserFailure err) = pure (ParserFailure err)
  traverse f (ParserSuccess x) = ParserSuccess <$> f x

instance Monad ParserResult where
  return = pure
  ParserFailure err >>= _ = ParserFailure err
  ParserSuccess x >>= f = f x

instance Applicative Parser where
  pure x = Parser $ \input -> (input, ParserSuccess x)
  Parser f <*> Parser fa = Parser $ \i ->
    case f i of
      (i', ParserSuccess g) -> case fa i' of
        (i'', r') -> (i'', fmap g r')
      (i', ParserFailure e) -> (i', ParserFailure e)

instance Alternative Parser where
  empty = failingParser "Empty parser always fails"
  (Parser p1) <|> (Parser p2) = Parser $ \i ->
    case p1 i of
      (i', ParserSuccess x) -> (i', ParserSuccess x)
      (_', ParserFailure _) -> p2 i

instance Monad Parser where
  Parser p >>= f = Parser $ \i ->
    case p i of
      (i', ParserSuccess x) -> let Parser q = f x in q i'
      (i', ParserFailure e) -> (i', ParserFailure e)

-- because we start on line 1 and we haven't consumed any characters
zeroLocation :: Location
zeroLocation = Location 1 0

shiftLocationByChar :: Location -> Char -> Location
shiftLocationByChar (Location l _) '\n' = Location (l + 1) (column zeroLocation)
shiftLocationByChar (Location l c) _ = Location l (c + 1)

shiftLocationByString :: T.Text -> Location -> Location
shiftLocationByString input loc = T.foldl shiftLocationByChar loc input

resultToMaybe :: ParserResult a -> Maybe a
resultToMaybe (ParserSuccess a) = Just a
resultToMaybe (ParserFailure _) = Nothing

errorToMaybe :: ParserResult a -> Maybe ParserError
errorToMaybe (ParserSuccess _) = Nothing
errorToMaybe (ParserFailure e) = Just e

failingParser :: ParserError -> Parser a
failingParser err = Parser $ \i -> (i, ParserFailure err)

succeedingParser :: a -> Parser a
succeedingParser a = Parser $ \i -> (i, ParserSuccess a)

runParser :: Parser a -> T.Text -> (Location, T.Text, ParserResult a)
runParser (Parser run) i =
  let ((loc, leftOver), res) = run (id, i)
   in (loc zeroLocation, leftOver, res)

isParserError :: ParserResult a -> Bool
isParserError (ParserFailure _) = True
isParserError (ParserSuccess _) = False

isParserSuccess :: ParserResult a -> Bool
isParserSuccess (ParserSuccess _) = True
isParserSuccess _ = False
