{-

Binary Tree: data structure in which each node can have a left child and a right child. They cannot have more than 2 children.
Common uses of binary trees are binary search trees, binary heaps, and Huffman coding.
-}

data Bintree a = Empty | Node a (Bintree a) (Bintree a) deriving (Show)

height :: Bintree a -> Int
height Empty = 0
height (Node _ lc rc) = 1 + max (height lc) (height rc)

t1 :: Bintree Int
t1 = Node 3 (Node 1 Empty Empty) (Node 2 Empty Empty)
