{-

Binary Tree: data structure in which each node can have a left child and a right child. They cannot have more than 2 children.
Common uses of binary trees are binary search trees, binary heaps, and Huffman coding.
-}

data Bintree a = TEmpty | TNode a (Bintree a) (Bintree a) deriving (Show)

height' :: Bintree a -> Int
height' TEmpty = 0
height' (TNode _ lc rc) = 1 + max (height' lc) (height' rc)

t1 :: Bintree Int
t1 = TNode 3 (TNode 1 TEmpty TEmpty) (TNode 2 TEmpty TEmpty)

{-
The definition of the tree is given by
    data Tree a = Node a (Tree a) (Tree a) | Empty deriving (Show)

    That is, a tree with elements of type a is, either an empty tree, either a node with an element of type a and two other trees
    of same type. The deriving (Show) statement simply enables visualization of the trees.
-}

data Tree a = Node a (Tree a) (Tree a) | Empty deriving (Show)

-- Write a function size :: Tree a -> Int that, given a tree, returns its size, that is, the number of nodes it contains.
size :: Tree a -> Int
size Empty = 0
size (Node _ lc rc) = 1 + size lc + size rc

-- size' :: Tree a -> Int
-- size' Empty = 0
-- size' (Node _ lc rc) = go lc rc 1
--   where
--     go :: Tree a -> Tree a -> Int -> Int
--     go Empty Empty sz = sz
--     go (Node _ lc rc) Empty sz = go lc rc (sz + 1)
--     go Empty (Node _ lc rc) sz = go lc rc (sz + 1)
--     go (Node _ lc1 rc1) (Node _ lc2 rc2) = go lc rc

tree :: (Tree Int, Tree Int)
tree =
  let t7 = Node 7 Empty Empty
      t6 = Node 6 Empty Empty
      t5 = Node 5 Empty Empty
      t4 = Node 4 Empty Empty
      t3 = Node 3 t6 t7
      t2 = Node 2 t4 t5
      t1 = Node 1 t2 t3
      t1' = Node 1 t3 t2
   in (t1, t1')

tree' :: (Tree Int, Tree Int)
tree' =
  let t8 = Node 8 Empty Empty
      t7 = Node 7 Empty Empty
      t5 = Node 5 t7 t8
      t6 = Node 6 Empty Empty
      t4 = Node 4 Empty Empty
      t3 = Node 3 t6 Empty
      t2 = Node 2 t4 t5
      t1 = Node 1 t2 t3
      t1' = Node 1 t3 t2
   in (t1, t1')

anotherTree :: Tree Int
anotherTree =
  let t2 = Node 2 Empty Empty
      t1 = Node 1 Empty Empty
      t9 = Node 9 Empty Empty
      t4 = Node 4 Empty t2
      t7 = Node 7 t1 t9
      t5 = Node 5 t4 t7
   in t5

stringTree :: Tree String
stringTree =
  let t3 = Node "pen" Empty Empty
      t4 = Node "pie" Empty Empty
      t6 = Node "table" Empty Empty
      t7 = Node "field" Empty Empty
      t2 = Node "car" t3 t4
      t5 = Node "book" t6 t7
      t1 = Node "sun" t2 t5
   in t1

height :: Tree a -> Int
height Empty = 0
height (Node _ lc rc) = 1 + max (height lc) (height rc)

-- Equivalent Trees
-- Write a function equal :: Eq a => Tree a -> Tree a -> Bool that, given two trees, tells whether they are the same
equal :: (Eq a) => Tree a -> Tree a -> Bool
equal Empty Empty = True
equal _ Empty = False
equal Empty _ = False
equal (Node a1 l1 r1) (Node a2 l2 r2) = a1 == a2 && equal l1 l2 && equal r1 r2

-- Isomorphism
-- Write a function isomorphic :: Eq a => Tree a -> Tree a -> Bool that, given two trees, tells whether they are isomorphic,
-- that is, if one can obtain one from the other by flipping some of its descendants.
isomorphic :: (Eq a) => Tree a -> Tree a -> Bool
isomorphic Empty Empty = True
isomorphic _ Empty = False
isomorphic Empty _ = False
isomorphic (Node a1 l1 r1) (Node a2 l2 r2) =
  a1 == a2 && (isomorphic l1 l2 && isomorphic r1 r2) || (isomorphic l1 r2 && isomorphic r1 l2)

-- Write a function preOrder :: Tree a  -> [a] that, given a tree, return it's pre-order traversal
preOrder' :: Tree a -> [a]
preOrder' t = go t []
  where
    -- This is a failed attempt at a tail recursive traversal of the binary tree.
    go :: Tree a -> [a] -> [a]
    go Empty ls = ls
    go (Node a l r) ls = go l (a : ls) ++ go r ls

preOrder :: Tree a -> [a]
preOrder Empty = []
preOrder (Node a l r) = (a : preOrder l) ++ preOrder r

-- Write a function postOrder :: Tree a -> [a] that given a tree, returns it's postOrder traversal
postOrder :: Tree a -> [a]
postOrder Empty = []
postOrder (Node a l r) = postOrder l ++ postOrder r ++ [a]

-- Write a function inOrder:: Tree a -> [a] that given a tree, returns it's inOrder traversal
inOrder :: Tree a -> [a]
inOrder Empty = []
inOrder (Node a l r) = inOrder l ++ [a] ++ inOrder r

-- Write a function breadthFirst :: Tree a -> [a] that, given a tree, returns its traversal by levels aka levelOrderTraversal
breadthFirst :: Tree a -> [a]
breadthFirst Empty = []
breadthFirst (Node a l r) = go [a] [l, r]
  where
    go :: [a] -> [Tree a] -> [a]
    go res [] = res
    go res (Empty : ns) = go res ns
    -- There can be additional criteria while visiting the children : always left, always right, lexical order etc.
    -- Here we are choosing left always.
    go res ((Node a l r) : ns) = go (res ++ [a]) (ns ++ [l, r])

breadthFirst' :: Tree a -> [a]
breadthFirst' t = bfs [t]
  where
    bfs :: [Tree a] -> [a]
    bfs [] = []
    bfs (Empty : ns) = bfs ns
    bfs ((Node a l r) : ns) = a : bfs (ns ++ [l, r])
