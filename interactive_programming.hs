{-
In Haskell, an interactive program is viewed as a pure function that takes the current state of the world as its argument, and produces a modified world as its
result, in which the modified world reflects any side effects that were performed by the program during its execution. Hence, given a suitable type World whose
values represent states of the world, the notion of an interactive program can be represented by a function of type World -> World, which can be abbreviated
as IO (Input/Output):
    type IO = World -> World

In general, however, an interactive program may return a result value in addition to performing side effects. For example, a program for reading a character from
the keyboard may return the character that was read. To accommodate this behavior, the IO is generalized on a type:

    type IO a = World -> (a, World)

Expressions of type IO a are called actions. For example, IO Char is the type of actions that return a character, while IO () is the type of actions that return the empty
tuple () as  dummy result value. Actions of IO () can be thought of as purely side-effecting actions that return no result value, and are often useful in interactive
programming.

Basic Actions

1. The action `getChar` reads a character from the keyboard, echoes it to the screen, and returns the character as its result value.

    getChar :: IO Char

2. The dual action `putChar c`, writes the character c to the screen and returns no result value, represented by the empty tuple:

    putChar :: Char -> IO ()

3. Final basic action is return v, which simply returns the result value v without performing any interaction with the user.

    return :: a -> IO a

The function `return` provides a bridge from pure expressions without side effects to impure actions with side effects. Crucially, there is no bridge back - once we are
impure we are impure for ever. For most Haskell programs, the vast majority of functions do not involve interaction, with this being handled by a relatively small
number of interactive functions at the outermost level.

Sequencing

In Haskell, a sequence of IO actions can be combined into a single composite action using the do notation:
    do v1 <- a1
         v2 <- a2
         .
         .
         .
         vn <- an
         return (f v1 v2 v3 . . . vn)
Such expressions have a simple operational reading: first perform the action a1 and call its result value v1; then perform the action a2 and call its result value v2; ...;
then perform the action an and call its result value vn; and finally, apply the function f to combine all the results into a single value, which is then returned as the
result value from the expression as a whole.

1. The layout rule applies, such that each action in the sequence must begin in precisely the same column.
2. As with list comprehensions, the expressions vi <- ai are called generators, because they generate values for the variables vi.
3. If the result value produced by a generator vi <- ai is not required, the generator can be abbreviated simply by ai, which has the same meaning as writing _ <- ai
-}

import Control.Monad (replicateM)
import Data.Char
import System.IO (hFlush, hSetEcho, stdin, stdout)

act :: IO (Char, Char)
-- An example function which skips the second character, and returns the first and third as a pair:
act = do
  x <- getChar
  getChar
  y <- getChar
  return (x, y)

-- Derived Primitives
getLine :: IO String
getLine = do
  x <- getChar
  if x == '\n'
    then
      return []
    else do
      xs <- Main.getLine
      return (x : xs)

putStr :: String -> IO ()
putStr [] = return ()
putStr (c : cs) = do
  putChar c
  Main.putStr cs

putStrLn :: String -> IO ()
putStrLn xs = do
  Main.putStr xs
  putChar '\n'

strlen :: IO ()
strlen = do
  Main.putStr "Enter a string: "
  xs <- Main.getLine
  Main.putStr "The string has "
  Main.putStr (show (length xs))
  Main.putStrLn " characters"

{-
Hangman:
At the start of the game, one player secretly enters a word. Another player then tries to deduce the word via a series of guesses. For each guess, we indicate
which lettres in the secret word occur in the game, and the game ends when the guess is correct.
-}
hangman :: IO ()
hangman = do
  Main.putStrLn "Think of a word:"
  word <- sgetLine
  Main.putStrLn "Try to guess it:"
  play word

-- sgetLine reads a string of characters from the keyboard in a similar manner to the basic getLine, except that it echoes each character as a dash symbol '-'
-- in order to keep the string secret.
sgetLine :: IO String
sgetLine =
  do
    x <- getCh
    if x == '\n'
      then do
        putChar x
        return []
      else do
        putChar '-'
        xs <- sgetLine
        return (x : xs)

-- The action getCh reads a single character from the keyboard without echoing it to the screen, and is defined by using the primitive hSetEcho from the library
-- System.IO

getCh :: IO Char
getCh = do
  hSetEcho stdin False
  x <- getChar
  hSetEcho stdin True
  return x

play :: String -> IO ()
play word = do
  Main.putStr "? "
  guess <- Main.getLine
  if guess == word
    then
      Main.putStrLn "You got it!"
    else do
      Main.putStrLn (match word guess)
      play word

match :: String -> String -> String
match xs ys = [if x `elem` ys then x else '-' | x <- xs]

{-
Nim
    1: * * * * *
    2: * * * *
    3: * * *
    4: * *
    5: *

    The players then take it in turn to remove one or more stars from the end of a single row. The winner is the player who
    makes the board empty, that is, who removes the final star or stars from the board.
-}

-- For simplicity, we represent the player number (1 or 2) as an Integer, and we use the following function to get the next player:
next :: Int -> Int
next 1 = 2
next 2 = 1

{-
In turn, we represent the board as a list comprising the number of stars that remain on each row, with the initial board given
by the list [5,4,3,2,1] and the game being finished when all rows have no stars left:
-}
type Board = [Int]

initial :: Board
initial = [5, 4, 3, 2, 1]

finished :: Board -> Bool
finished = all (== 0)

{-
A move in the game is specified by a row number and the number of stars to be removed, and is valid if the row contains at least
this many stars.
-}
valid :: Board -> Int -> Int -> Bool
valid b r n = b !! (r - 1) >= n

move :: Board -> Int -> Int -> Board
move board row num = [update r n | (r, n) <- zip [1 ..] board]
  where
    update r n = if r == row then n - num else n

putRow :: Int -> Int -> IO ()
putRow row num =
  do
    Prelude.putStr (show row)
    Prelude.putStr ": "
    Prelude.putStrLn (concat (replicate num "* "))

putBoard :: Board -> IO ()
putBoard [a, b, c, d, e] =
  do
    putRow 1 a
    putRow 2 b
    putRow 3 c
    putRow 4 d
    putRow 5 e

getDigit :: String -> IO Int
getDigit prompt =
  do
    Prelude.putStr prompt
    x <- getChar
    newline
    if isDigit x
      then
        return (digitToInt x)
      else do
        Prelude.putStrLn "ERROR: Invalid digit"
        getDigit prompt

newline :: IO ()
newline = Prelude.putChar '\n'

-- Game of Nim
playNim :: Board -> Int -> IO ()
playNim board player =
  do
    newline
    -- putBoard board
    -- betterPutBoard board
    anotherPutBoard board
    if finished board
      then do
        newline
        Prelude.putStr "Player"
        Prelude.putStr (show (next player))
        Prelude.putStrLn " wins!!"
      else do
        newline
        Prelude.putStr "Player"
        Prelude.putStrLn (show player)
        row <- getDigit "Enter a row number: "
        num <- getDigit "Stars to remove: "
        if valid board row num
          then
            playNim (move board row num) (next player)
          else do
            newline
            Prelude.putStrLn "ERROR: Invalid move"
            playNim board player

nim :: IO ()
nim = playNim initial 1

{-
Game of Life:
    The game models a simple evolutionary system based on cells, and is played on a two-dimensional board. Each square
    on the board is either empty, or contains a single living cell. Each internal square on the board has eight internal neighbors.

    For uniformity, each external square on the board is also viewed as having eight neighbors, by assuming that the board wraps
    around from top-to-bottom and from left-to-right. That is, we can think of the board as really being a torus, the surface of a
    three-dimensional doughnut shaped object.

    Given an initial configuration of the board, the next generation of the board is given by simultaneously applying the following
    rules to all squares:
        * a living cell survives if it has precisely two or three neighbouring squares that contain living cells, and
        * an empty square gives birth to a living cell if it has precisely three neighbours that contain living cells, and remains empty otherwise.

    Despite it's simplicity, the game of life is in fact computationally complete, in the sense that any computational process can be
    simulated within it by means of a suitable encoding.
-}
-- Screen Utilities
cls :: IO ()
cls = Prelude.putStr "\ESC[2J"

type Pos = (Int, Int)

-- Function to display a string at a given position on the screen.
writeat :: Pos -> String -> IO ()
writeat p xs =
  do
    goto p
    Prelude.putStr xs

goto :: Pos -> IO ()
goto (x, y) = Prelude.putStr ("\ESC[" ++ show y ++ ";" ++ show x ++ "H")

width :: Int
width = 10

height :: Int
height = 10

type LifeBoard = [Pos]

glider :: LifeBoard
glider = [(4, 2), (2, 3), (4, 3), (3, 4), (4, 4)]

-- The library function sequence_ :: [IO a] -> IO () performs a list of IO actions in sequence, discarding their results.
showCells :: LifeBoard -> IO ()
showCells b = sequence_ [writeat p "O" | p <- b]

isAlive :: LifeBoard -> Pos -> Bool
isAlive b p = p `elem` b

isEmpty :: LifeBoard -> Pos -> Bool
isEmpty b p = not (isAlive b p)

neighbs :: Pos -> [Pos]
neighbs (x, y) =
  map
    wrap
    [ (x - 1, y - 1),
      (x, y - 1),
      (x + 1, y - 1),
      (x - 1, y),
      (x + 1, y),
      (x - 1, y + 1),
      (x, y + 1),
      (x + 1, y + 1)
    ]

wrap :: Pos -> Pos
wrap (x, y) =
  ( ((x - 1) `mod` width) + 1,
    ((y - 1) `mod` height) + 1
  )

-- A function that calculates the number of living positions in a board that have precisely two or three neighbours, and
-- hence survive to the next generation of the game:
liveneighbs :: LifeBoard -> Pos -> Int
liveneighbs b = length . filter (isAlive b) . neighbs

survivors :: LifeBoard -> [Pos]
survivors b = [p | p <- b, (liveneighbs b p) `elem` [2, 3]]

births :: LifeBoard -> [Pos]
births b =
  [ (x, y)
  | x <- [1 .. width],
    y <- [1 .. height],
    isEmpty b (x, y),
    liveneighbs b (x, y) == 3
  ]

birthsEfficient :: LifeBoard -> [Pos]
birthsEfficient b =
  [ p
  | p <- rmdups (concatMap neighbs b),
    isEmpty b p,
    liveneighbs b p == 3
  ]

rmdups :: (Eq a) => [a] -> [a]
rmdups [] = []
rmdups (x : xs) = x : rmdups (filter (/= x) xs)

nextgen :: LifeBoard -> LifeBoard
nextgen b = survivors b ++ births b

life :: LifeBoard -> IO ()
life b = do
  cls
  showCells b
  wait 50000
  life (nextgen b)

-- A simple function which returns a list of empty actions based on the input number provided.
wait :: Int -> IO ()
wait n = sequence_ [return () | _ <- [1 .. n]]

-- Exercises

{-
Redefine putStr :: String -> IO () using a list comprehension and the library function sequence_ :: [IO a] -> IO ()
-}
myPutStr :: String -> IO ()
myPutStr s = sequence_ [putChar c | c <- s]

{-
Using recursion, define a version of putBoard :: Board -> IO () that displays nim boards of any size, rather than being
specific to boards with just five rows of stars.
Hint: First define an auxiliary function that takes the current row number as an additional argument.
-}
anotherPutBoard :: Board -> IO ()
anotherPutBoard b = showBoard 1 b
  where
    showBoard :: Int -> Board -> IO ()
    showBoard _ [] = return ()
    showBoard i (p : ps) = do
      putRow i p
      showBoard (i + 1) ps

{-
In a similar manner to the first exercise, redefine the generalized version of putBoard using a list comprehension and sequence_
-}
betterPutBoard :: Board -> IO ()
betterPutBoard b = sequence_ [putRow i n | (i, n) <- zip [1 ..] b]

{-
Define an action adder :: IO () that reads a given number of integers from the keyboard, one per line, and displays their sum. For e.g.:

> adder
How many numbers ? 5
1
3
5
7
9
The total is 25

Hint: start by defining an auxiliary function that takes the current total and how many numbers remain to be read as arguments.
You will also likely need to use the library functions read and show
-}
adder :: IO ()
adder =
  do
    Prelude.putStr "How many numbers?"
    hFlush stdout -- Send output immediately to the screen.
    n <- readLn
    -- list_numbers <- replicateM n (readLn :: IO Int)
    s <- repeat n 0
    Prelude.putStrLn ("The total is " ++ show s)
  where
    repeat :: Int -> Int -> IO Int
    repeat n s =
      if n > 0
        then do
          v <- readLn
          repeat (n - 1) (s + v)
        else
          return s

{-
Redefine adder using the function sequence :: [IO a] -> IO [a] that performs a list of actions and returns a list of resulting values.
-}
anotherAdder :: IO ()
anotherAdder =
  do
    Prelude.putStr "How many numbers?"
    hFlush stdout
    n <- readLn
    let list_numbers = sequence [(readLn :: IO Int) | _ <- [1 .. n]]
    nums <- list_numbers
    Prelude.putStrLn ("The total is " ++ show (sum nums))

{-
Using getCh, define an action readLine :: IO String that behaves in the same way as getLine, except that it also permits the delete
key to be used to remove characters.
Hint: the delete character is `\DEL`, and the control characters for moving the cursor back one space is `\b`.
-}
readLine :: IO String
readLine = go []
  where
    removeHead :: [Char] -> [Char]
    removeHead [] = []
    removeHead (_ : xs) = xs

    go :: [Char] -> IO String
    go acc =
      do
        c <- getCh
        if c == '\DEL'
          then go (removeHead acc)
          else
            if c == '\n'
              then return (reverse acc)
              else
                go (c : acc)
