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
acyclicPaths :: Eq a => a -> a -> [(a,a)] -> [[a]]
acyclicPaths source sink edges
    | source == sink = [[sink]]
    | otherwise = [
        source:path | edge <- edges, (fst edge) == source,
            path <- (acyclicPaths (snd edge) sink [e | e <- edges, e/=edge])
    ]


-- Write a function depthFisrt :: Graph -> Node -> [Node] that generates a depth-first order graph traversal sequence Use the following graph notation:
type Node = Int
type Edge = (Node, Node)
type Graph = ([Node], [Edge])

depthFirst :: Graph -> Node -> [Node]
