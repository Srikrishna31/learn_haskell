import Data.Char
import Data.List
import System.IO

size :: Int
size = 3

{-
Recall that the ordering on constructors is determined by their position in the data declaration, hence we have O < B < X,
which will be important for the minimax algorithm.
-}
data Player = O | B | X deriving (Eq, Ord, Show)

type Grid = [[Player]]

{-
The next player to move is given simply by swapping between 0 and X, with the case for the blank value B being included
for completeness even though the function should never be applied to this value.
-}
next :: Player -> Player
next O = X
next B = B
next X = O

-- Grid utility functions
empty :: Grid
empty = replicate size (replicate size B)

full :: Grid -> Bool
-- full = all (/= B) . concat
full = all (all (/= B))

-- Custom concat implementation without building intermediate lists
myConcat :: [[a]] -> [a]
myConcat [] = []
myConcat ([] : xss) = myConcat xss -- Skip empty sublists
myConcat ((x : xs) : xss) = x : myConcat (xs : xss) -- Stream elements directly

turn :: Grid -> Player
turn g = if os <= xs then O else X
  where
    os = length (filter (== O) ps)
    xs = length (filter (== X) ps)
    ps = myConcat g

{-
A game has been won, if a player has a complete line in any row, column or either diagonal in the grid.
-}
wins :: Player -> Grid -> Bool
wins p g = any line (rows ++ cols ++ dias)
  where
    line = all (== p)
    rows = g
    cols = transpose g
    dias = [diag g, diag (map reverse g)]

-- Diag function returns the main diagonal of the grid. The other diagonal, from top-right to bottom-left
-- can then be obtained by first reversing each row in the grid.
diag :: Grid -> [Player]
diag g = [g !! n !! n | n <- [0 .. size - 1]]

won :: Grid -> Bool
won g = wins O g || wins X g

-- Displaying a grid
{-
We convert each row to a list of strings using showRow, insert a horizontal bar between each row using interleave,
flatten the resulting nested list structure using concat/myConcat, join all the strings together with a newline character at
the end of each line using the library function unlines :: [String] -> String, and finally display the resulting string on the screen
using putStrLn.
-}
putGrid :: Grid -> IO ()
putGrid =
  putStrLn . unlines . myConcat . interleave bar . map showRow
  where
    bar = [replicate ((size * 4) - 1) '-']

{-
The library function foldr1 used below behaves in a similar manner to foldr but can only be applied to non-empty lists, while
zipWith behaves in the same way as zip but applies a given function to each pair of values in the resulting list.
-}
showRow :: [Player] -> [String]
showRow = beside . interleave bar . map showPlayer
  where
    beside = foldr1 (zipWith (++))
    bar = replicate 3 "|"

showPlayer :: Player -> [String]
showPlayer O = ["   ", " O ", "   "]
showPlayer B = ["   ", "   ", "   "]
showPlayer X = ["   ", " X ", "   "]

-- function to interleave a value between every element in the list.
interleave :: a -> [a] -> [a]
interleave _ [] = []
interleave _ [y] = [y]
interleave x (y : ys) = y : x : interleave x ys

{-
Making a move

To identify where a player wishes to make a move during the game, we index each position in the grid by a natural number
starting from zero in the top-left corner and proceeding along each row in turn. Attempting to make a move at a particular
index is valid if the index is within the appropriate range, and the position is currently blank.
-}
valid :: Grid -> Int -> Bool
valid g i = 0 <= i && i < size ^ 2 && concat g !! i == B

{-
Move
In order to take account of the possibility that a move may be invalid, we return a list of grids as the result, with the convention
that a singleton list denotes success in applying the move, and the empty list denotes failure:
That is, if the move is valid we split the list of player values in the grid at the index where the move is being made, replace
the blank player value with the given player, and then reform the grid once again.
-}
move :: Grid -> Int -> Player -> [Grid]
move g i p =
  -- if valid g i then [chop size (xs ++ [p] ++ ys)] else []
  [chop size (xs ++ [p] ++ ys) | valid g i]
  where
    (xs, B : ys) = splitAt i (concat g)

chop :: Int -> [a] -> [[a]]
chop _ [] = []
chop n xs = take n xs : chop n (drop n xs)

getNat :: String -> IO Int
getNat prompt =
  do
    putStr prompt
    xs <- getLine
    if xs /= [] && all isDigit xs
      then
        return (read xs)
      else do
        putStrLn "ERROR: Invalid number"
        getNat prompt

{-
Human vs Human
-}

cls :: IO ()
cls = putStr "\ESC[2J"

goto :: (Int, Int) -> IO ()
goto (x, y) = putStr ("\ESC[" ++ show y ++ ";" ++ show x ++ "H")

tictactoe :: IO ()
tictactoe = run empty O

run :: Grid -> Player -> IO ()
run g p =
  do
    cls
    goto (1, 1)
    putGrid g
    run' g p

run' :: Grid -> Player -> IO ()
run' g p
  | wins O g = putStrLn "Player O wins!\n"
  | wins X g = putStrLn "Player X wins!\n"
  | full g = putStrLn "It's a draw!\n"
  | otherwise =
      do
        i <- getNat (prompt p)
        case move g i p of
          [] -> do
            putStrLn "ERROR: Invalid move"
            run' g p
          [g'] -> run g' (next p)

prompt :: Player -> String
prompt p = "Player " ++ show p ++ ", enter your move: "

{-
Game trees
The basic idea is to build a tree structure that captures all possible ways in which the game can proceed from the current
grid, and then use this tree to decide on the best next move to make
The below declaration gives a suitable representation for tic-tac-toe game trees.
\* Firstly, the declaration is not specific to tic-tac-toe grids, but permits any type of values to be stored in the nodes; this
becomes important when we consider the minimax algorithm, which labels each grid in the game tree with additional
information.
\* Secondly, there is no constructor for leaves, because a node with an empty list of subtrees can play this role; this avoids
having two possible representations for leaves, which could complicate the definition of functions on trees.
-}
data Tree a = Node a [Tree a] deriving (Show)

{-
Using the above tree type, it is straightforward to define a function that builds a game tree from a given starting grid and
player. We simply use the starting grid as the value for the root node, and then recursively build a game tree for each grid
that results from the current player making a valid move, with the next player then being used to continue the process:
-}
gametree :: Grid -> Player -> Tree Grid
gametree g p = Node g [gametree g' (next p) | g' <- moves g p]

{-
The function moves that returns the list of valid moves, is defined by first checking if the game is finished, in which case
we return the empty list of grids, which serves to stop the recursion in gametree. Otherwise, we return all grids that result
from making a move in a blank space:
-}
moves :: Grid -> Player -> [Grid]
moves g p
  | won g = []
  | full g = []
  | otherwise = concat [move g i p | i <- [0 .. ((size ^ 2) - 1)]]

{-
Game trees can potentially become very large. For this reason, it is sometimes necessary to prune game trees to a particular
depth, in order to limit the amount of time and memory that it takes to build the tree.
For example, prune 5 (gametree empty O) produces a game tree of maximum depth five starting from the empty grid with
player O making the first move. Note that, under lazy evaluation, only as much of the tree as required by the prune function
will actually be produced. That is, grids beyond depth five in this example will never be generated by gameTree.
-}
prune :: Int -> Tree a -> Tree a
prune 0 (Node x _) = Node x []
prune n (Node x ts) = Node x [prune (n - 1) t | t <- ts]

{-
We also define a constant that specifies the maximum depth of the game tree. On a modern machine, it is feasible to generate,
the entire tree for a 3x3 grid, so we set the default depth to the maximum value required for grids of this size. For larger grids
it may be necessary to reduce this value.
-}
depth :: Int
depth = 9

{-
Minimax algorithm
The algorithm start by labelling every node in the tree with a player value in the following manner:
    * Leave (nodes with no subtrees) are labelled with the winning player at this point if there's one, and the blank player otherwise.
    * Other nodes (with subtrees) are labelled with the minimum or maximum of the player labels from the child nodes one level
    down, depending on whose turn it is to move at this point: on player O's turn we take the minimum of the child labels, and on
    X's turn we take the maximum.

Using a series of guards to determine the label, the minimax algorithm can be translated into a function that labels a game tree,
where the local definition ts' applies the algorithm recursively to each subtree of a node, and ps selects the top labels from the
resulting trees.
-}
minimax :: Tree Grid -> Tree (Grid, Player)
minimax (Node g [])
  | wins O g = Node (g, O) []
  | wins X g = Node (g, X) []
  | otherwise = Node (g, B) []
minimax (Node g ts)
  | turn g == O = Node (g, minimum ps) ts'
  | turn g == X = Node (g, maximum ps) ts'
  where
    ts' = map minimax ts
    ps = [p | Node (_, p) _ <- ts']

{-
Once the game tree has been labelled in this manner, the best next move under the minimax algorithm is given by moving to any
grid with the same label as the root node.
That is, we first build the game tree up to the specified dpeth, then apply the minimax algorithm to label the tree, and finally select
a grid whose player label is the same as that of the root node. There is always at least one 'best move', because selecting the minimum
or maximum value from a non-empty (finite) list always results in a value that occurs in the list. If there is more than one best move,
the algorithm simply selects the first of these.
-}
bestMove :: Grid -> Player -> Grid
bestMove g p = head [g' | Node (g', p') _ <- ts, p' == best]
  where
    tree = prune depth (gametree g p)
    Node (_, best) ts = minimax tree

{-
Human vs Computer
The function hSetBuffering is provided in the library System.IO, and is used above to turn output buffering off, which is by default
turned on in GHC.
-}
main :: IO ()
main = do
  hSetBuffering stdout NoBuffering
  play empty O

play :: Grid -> Player -> IO ()
play g p = do
  cls
  goto (1, 1)
  putGrid g
  play' g p

{-
The operator $! used below for p==X branch, forces evaluation of the best move for the computer player prior to the function
play being invoked again, without which there may be a delay between clearing the screen and displaying the grid in play while
the best move was then calculated under lazy evaluation.
-}
play' :: Grid -> Player -> IO ()
play' g p
  | wins O g = putStrLn "Player O wins!\n"
  | wins X g = putStrLn "Player X wins!\n"
  | full g = putStrLn "It's a draw!\n"
  | p == O = do
      i <- getNat (prompt p)
      case move g i p of
        [] -> do
          putStrLn "ERROR: Invalid move"
          play' g p
        [g'] -> play g' (next p)
  | p == X = do
      putStr "Player X is thinking..."
      (play $! bestMove g p) (next p)
