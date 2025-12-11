{-
    Haskell Graphs Notation

    Edge notation

    data Graph a = Edge [(a,a)] deriving (Show, Eq)

    Problem with Edge notation is that, isolated nodes cannot be represented.

    Representation by Nodes and its edges

    data Graph a = Graph [a] [(a,a)] deriving (Show, Eq)

    Adjacency List Notation

    data Graph a = Adj [(a, [a])] deriving (Show, Eq)
-}

{-
Write a function acyclicPaths :: Eq a => a -> a -> [(a,a)] -> [[a]] that given two nodes a and b in a graph, returns all the acyclic paths from a to b.

In graph theory, a cycle in a graph is a non-empty trail in which only the first and last vertices are equal.
-}
acyclicPaths :: (Eq a) => a -> a -> [(a, a)] -> [[a]]
acyclicPaths source sink edges
  | source == sink = [[sink]]
  | otherwise =
      [ source : path
      | edge <- edges,
        (fst edge) == source,
        path <- (acyclicPaths (snd edge) sink [e | e <- edges, e /= edge])
      ]

-- Write a function depthFisrt :: Graph -> Node -> [Node] that generates a depth-first order graph traversal sequence Use the following graph notation:
{-
The algorithm starts at a starting node and explores as far as possible along each branch before backtracking

depthFirst ([1,2,3,4,5,6,7], [(1,2), (2,3), (1,4), (3,4), (2,5), (5,4), (6,7)]) 1
-}
type Node = Int

type Edge = (Node, Node)

type Graph = ([Node], [Edge])

depthFirst :: Graph -> Node -> [Node]
depthFirst (v, e) node
  | null [x | x <- v, x == node] = [] -- if the set of nodes is empty, return empty list.
  | otherwise = depthrec (v, e) [node]
  where
    depthrec :: Graph -> [Node] -> [Node]
    depthrec ([], _) _ = []
    depthrec (_, _) [] = []
    depthrec (v, e) (top : stack)
      | null [x | x <- v, x == top] = depthrec (newv, e) stack
      | otherwise = top : depthrec (newv, e) (adjacent ++ stack)
      where
        adjacent = [x | (x, y) <- e, y == top] ++ [x | (y, x) <- e, y == top]
        newv = [x | x <- v, x /= top]

{-
Write a predicate `connectedComponents :: Graph -> [[Node]]` that splits a graph into its
connected components.`

connectedComponents  ([1,2,3,4,5,6,7], [(1,2), (2,3), (1,4), (3,4), (5,2), (5,4), (6,7)])
[[1,2,5,4,3],[6,7]]
-}

connectedComponents :: Graph -> [[Node]]
connectedComponents ([], _) = []
connectedComponents (v, e) = dfs_res : connectedComponents ([x | x <- v, x `notElem` dfs_res], e)
  where
    dfs_res = depthFirst (v, e) (head v)
