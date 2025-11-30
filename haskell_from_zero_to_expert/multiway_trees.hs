{-
    Multiway Trees

    A multiway tree is composed of a root element and a (possibly empty) set of successors which are multiway trees themselves.
        - A multiway tree is never empty
        - The set of successor trees is sometimes called a forest
-}

data Tree a = Node a [Tree a] deriving (Eq, Show)

tree1 :: Tree Char
tree1 =
  Node
    'a'
    [ Node 'f' [Node 'g' []],
      Node 'c' [],
      Node 'b' [Node 'd' [], Node 'e' []]
    ]

-- Write a function numnodes :: Tree a -> Int that, given a multiway tree, returns the number of nodes.
numnodes :: Tree a -> Int
numnodes (Node _ ns) = 1 + sum (map numnodes ns)

-- Write a function stringToTree :: String -> Tree a that, given a string, it builds the tree. We suppose that the nodes of a multiway tree contain single characters.
-- In the depth-first order sequence of its nodes, a special character ^ has been inserted whenerver, during the tree traversal, the move is a backtrack to the
-- previous level.
-- By this rule, the tree1 is represented as: afg^^c^bd^e^^^
stringToTree :: String -> Tree Char
stringToTree [s]
  | s /= '^' = Node s []
stringToTree (s : ss)
  | s == '^' = stringToTree ss
  | otherwise = Node s [stringToTree ss]

stringToTree' :: String -> Tree Char
stringToTree' (x : xs) = Node x (fst (stringToTrees xs))
  where
    stringToTrees :: [Char] -> ([Tree Char], [Char])
    stringToTrees (x : xs)
      | x == '^' = ([], xs)
      | otherwise = (Node x trees0 : trees1, rest1)
      where
        (trees0, rest0) = stringToTrees xs
        (trees1, rest1) = stringToTrees rest0

-- Defining new operators in Haskell.
(%) :: Int -> Int -> Int
(%) = mod

-- Write a function pathLength :: Tree a -> Int that, given a multiway tree, it returns the path length of the tree.
-- We define the path length of a multiway tree as the total sum of the path lengths from the root to all nodes of the tree.
pathLength :: Tree a -> Int
pathLength n = go [(n, 0)] 0
  where
    go :: [(Tree a, Int)] -> Int -> Int
    go [] s = s
    go ((Node _ cs, v) : ns) s = go (ns ++ (map (\x -> (x, v + 1)) cs)) (s + v)

pathLength' :: Tree a -> Int
pathLength' = pathLengthAux 0
  where
    pathLengthAux d (Node _ c) = d + sum (map (pathLengthAux (d + 1)) c)

-- Write a function bottomUp :: Tree a -> [a] that constructs the bottom up sequence of the nodes of the multiway tree.
bottomUp :: Tree a -> [a]
bottomUp n = go [] [n]
  where
    go :: [a] -> [Tree a] -> [a]
    go as [] = as
    -- go as ((Node a cs) : ns) = as ++ go as (cs ++ ns) ++ [a]
    go as ((Node a cs) : ns) = (go as (reverse cs ++ ns)) ++ [a] -- This lists the children in left-to-right order
    -- go as ((Node a cs):ns) = go as (cs ++ ns ) ++ [a]  -- This lists the children in right-to-left order.

bottomUp' :: Tree a -> [a]
bottomUp' (Node a cs) = concatMap bottomUp' cs ++ [a]
