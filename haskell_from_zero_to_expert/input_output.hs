{-
    Input/Output in Haskell is based on a monad:

        * The main program is main::IO ()
        * The IO type constructor is used to handle input/output
        * IO is instance of Monad.
        * It is usually used with do notation.

        () is a zero-field tuple and () is the only value of type (). <=> void of C.

        Some basic operations:

        getChar :: IO Char
        getLine :: IO String
        getContents :: IO String

        putChar :: Char -> IO ()
        putStr :: String -> IO ()
        putStrLn :: String -> IO ()
        print :: Show a => a -> IO ()
-}

-- main :: IO ()
-- main = do
--   putStrLn "What is your name?"
--   name <- getLine
--   putStrLn $ "Hello " ++ name ++ "!"

-- Write a text backwards
main :: IO ()
main = do
  putStrLn "Enter any text"
  x <- getLine
  if last x /= '*'
    then do
      putStrLn $ reverse x
      main
    else
      return ()

-- putStrLn x
-- let y = reverse x
--  in putStrLn y
