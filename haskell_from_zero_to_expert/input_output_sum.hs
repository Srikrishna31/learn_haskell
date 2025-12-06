{-
Write an IO  program which will first read a positive integer, say n, and then reads n integers and writes their sum.
The program should prompt appropriately for its inputs and explain its output.
-}

import Data.List
import System.IO

sumOfNumbers :: IO ()
sumOfNumbers =
  do
    putStrLn "Compute the sum of some numbers."
    prompt "How many numbers?"
    n <- readLn
    let ask n = do
          prompt "Enter a number: "
          readLn
    list_numbers <- mapM ask [1 .. n]
    putStr "The sum of the numbers is "
    print (sum list_numbers)

prompt :: String -> IO ()
prompt t =
  do
    putStr t
    hFlush stdout -- Send the output immediately to the screen.

{-
    mapM :: Monad m => (a -> m b) -> t a -> m (t b)

    By default, output is buffered until you print a new line.
-}

{-
Write a program which repeatedly reads integers (one per line) until finding a zero value and outputs a sorted version of the
inputs read.
-}
conditionalSort :: IO [Int]
conditionalSort =
  do
    -- By using prompt, we flush the extraneous string characters out of the buffer, so that the following
    -- readLn will only see a number which it can convert.
    prompt "Enter a number (0 to end) "
    n <- readLn
    if n /= 0
      then do
        rest <- conditionalSort
        return $ sort (n : rest)
      else return []

conditionalSortImproved :: IO [Int]
conditionalSortImproved =
  -- do
  --   ns <- readUntilZero
  --   return $ sort ns
  do sort <$> readUntilZero
  where
    readUntilZero :: IO [Int]
    readUntilZero = do
      prompt "Enter a number (0 to end) "
      n <- readLn
      if n /= 0
        then do
          rest <- readUntilZero
          return (n : rest)
        else return []

{-
In this problem, you are going to implement the "number guessing game".  Here's an example of how it works:

Think of a number between 1 and 100!
Is it 50? higher
Is it 75? lower
Is it 62? lower
Is it 56? Yes
Great I won!

Implement a function

game :: IO ()
That plays one instance of this game.

You might need the following predefined functions:

getLine :: IO String
putStrLn :: String -> IO ()
-}
game :: IO ()
game =
  do
    putStrLn "Think of a number between 1 and 100!"
    play 1 100

play :: Int -> Int -> IO ()
play lo hi | lo > hi = putStrLn "This is not possible!"
play lo hi =
  do
    let mid = (lo + hi) `div` 2
    ans <- validM (`elem` ["lower", "higher", "yes"]) $
      do
        prompt ("Is it" ++ show mid ++ "? ")
        getLine
    case ans of
      "yes" -> putStrLn "Great, I won!"
      "lower" -> play lo (mid - 1)
      "higher" -> play (mid + 1) hi

validM :: (a -> Bool) -> IO a -> IO a
validM valid ask =
  do
    ans <- ask
    if valid ans
      then return ans
      else do
        putStrLn "Answer properly!"
        validM valid ask
