{-# LANGUAGE MultiParamTypeClasses #-}

module Model.Injection where

class Injection a b where
  to :: a -> b
