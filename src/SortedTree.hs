module SortedTree where

data Tree = Leaf | Node Int Tree Tree deriving Show

insert :: Tree -> Int -> Tree
insert Leaf v = Node v Leaf Leaf
insert (Node x left right) v | v == x    = Node x left right  -- ignore
                             | v < x     = Node x (insert left v) right
                             | otherwise = Node x left (insert right v)

toList :: Tree -> [Int]
toList Leaf = []
toList (Node x left right) = toList left ++ [x] ++ toList right
