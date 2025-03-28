{-# LANGUAGE DeriveFunctor #-}

module Model.Tree where


maybeLastAndRest :: [a] -> ([a], Maybe a)
maybeLastAndRest [] = ([], Nothing)
maybeLastAndRest [x] = ([], Just x)
maybeLastAndRest (x : xs) = (x : rest, l)
 where
  (rest, l) = maybeLastAndRest xs

maybeToEither :: Maybe a -> b -> Either b a
maybeToEither Nothing e = Left e
maybeToEither (Just a) _ = Right a

-- Forest is represented as a "root" empty node and children of it are root
-- nodes of trees in that forest
data Forest a = Forest [Forest a] | Node a [Forest a] deriving (Show, Functor)


leaf :: a -> Forest a
leaf a = Node a []

-- Appends new tree to a node
-- Level 0 means under root node
appendToAtLevel :: Forest a -> Forest a -> Int -> Either String (Forest a)
appendToAtLevel tree node level
  | level == 0 = case tree of
      Forest nodes -> Right $ Forest (nodes ++ [node])
      Node a nodes -> Right $ Node a (nodes ++ [node])
  | level > 0 = case tree of
      Forest nodes -> do
        let (rest, maybeLast) = maybeLastAndRest nodes
        l <- maybeToEither maybeLast "Can only add nodes to level 0 in empty forest"
        appendedLast <- appendToAtLevel l node (level - 1)
        Right $ Forest (rest ++ [appendedLast])
      Node a nodes -> do
        let (rest, maybeLast) = maybeLastAndRest nodes
        l <- maybeToEither maybeLast "Can only add nodes to level 0 in empty node"
        appendedLast <- appendToAtLevel l node (level - 1)
        Right $ Node a (rest ++ [appendedLast])
  | otherwise = Left "Cannot insert at negative level"

makeForest :: (Show a) => [a] -> (a -> Int) -> Either String (Forest a)
makeForest list getLevel = foldl append (Right $ Forest []) list
 where
  append (Left e) _ = Left e
  append (Right acc) value = appendToAtLevel acc (leaf value) (getLevel value)
