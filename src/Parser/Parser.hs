{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE OverloadedStrings #-}
{-# HLINT ignore "Use tuple-section" #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}

{-# HLINT ignore "Use ==" #-}

module Parser.Parser where

import qualified Data.Text as T
import GHC.Base

------------------------------- PARSER ---------------------------------------

data Location = Location {line :: Int, column :: Int} deriving (Show)
zeroLocation :: Location
zeroLocation = Location 0 0

shifLocationByChar :: Location -> Char -> Location
shifLocationByChar (Location l _) '\n' = Location (l + 1) 0
shifLocationByChar (Location l c) _ = Location l (c + 1)

shiftLocationByString :: T.Text -> Location -> Location
shiftLocationByString input loc = T.foldl shifLocationByChar loc input

type ParserInput = (Location -> Location, T.Text)
type ParserError = String
data ParserResult a = ParserSuccess a | ParserFailure ParserError deriving (Functor, Show)

instance Applicative ParserResult where
  pure = ParserSuccess
  (ParserSuccess f) <*> (ParserSuccess a) = ParserSuccess (f a)
  (ParserFailure e) <*> _ = ParserFailure e
  _ <*> (ParserFailure e) = ParserFailure e

newtype Parser a = Parser (ParserInput -> (ParserInput, ParserResult a)) deriving (Functor)

failingParser :: ParserError -> Parser a
failingParser err = Parser $ \i -> (i, ParserFailure err)

runParser :: Parser a -> T.Text -> (Location, T.Text, ParserResult a)
runParser (Parser run) i =
  let ((loc, leftOver), res) = run (id, i)
   in (loc zeroLocation, leftOver, res)

instance Applicative Parser where
  pure x = Parser $ \input -> (input, ParserSuccess x)
  Parser f <*> Parser fa = Parser $ \i ->
    let (i', r) = f i
        (i'', r') = fa i'
     in (i'', r <*> r')

instance Alternative Parser where
  empty = failingParser "Empty parser always fails"
  (Parser p1) <|> (Parser p2) = Parser $ \i ->
    let (i1, r1) = p1 i
        (i2, r2) = p2 i
     in case (r1, r2) of
          (ParserSuccess r, _) -> (i1, ParserSuccess r)
          (ParserFailure _, ParserSuccess r) -> (i2, ParserSuccess r)
          (ParserFailure e, ParserFailure _) -> (i1, ParserFailure e) -- TODO: in this case it is not clear which input to provide, but actually doesn't matter because it already failed

instance Monad Parser where
  p >>= f = flatten notFlattened
   where
    notFlattened = fmap f p
    flatten :: Parser (Parser b) -> Parser b
    flatten (Parser p1) = Parser $ \i -> case p1 i of
      (i', ParserSuccess (Parser p2)) -> p2 i'
      (i', ParserFailure e) -> (i', ParserFailure e)
