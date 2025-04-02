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

newtype Parser a = Parser (ParserInput -> (ParserInput, ParserResult a)) deriving (Functor)

failingParser :: ParserError -> Parser a
failingParser err = Parser $ \i -> (i, ParserFailure err)

succeedingParser :: a -> Parser a
succeedingParser a = Parser $ \i -> (i, ParserSuccess a)

runParser :: Parser a -> T.Text -> (Location, T.Text, ParserResult a)
runParser (Parser run) i =
  let ((loc, leftOver), res) = run (id, i)
   in (loc zeroLocation, leftOver, res)

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
