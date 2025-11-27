{-
Higher Order Functions

A Higher Order Function (HOF) is a function that receives or returns functions.

Key Point: functions are first class objects.
-}

-- map :: (a -> b) -> [a] -> [b]

map :: (a -> b) -> [a] -> [b]
map f [] = []
map f (x : xs) = f x : Main.map f xs

(.) :: (b -> c) -> (a -> b) -> (a -> c)
(f . g) x = f (g x)

-- apli2 function applies a function to an element twice
apli2 :: (a -> a) -> a -> a
-- apli2 f x = f (f x)
apli2 f = f Main.. f

{-
Anonymous Functions

Anonymous Functions (\lambda functions) are expressions that represent a function without name.

Anonymous functions are usually used when they are short and only used once. They are also useful for performing programming trasnformations.

Sections allow partial infix operators to be applied.

On the right:                                                                  On the left:

( . y) = \x -> x . y                                                              (y .) = \x -> y . x

-}

-- Implement a function eql::[Int] -> [Int] -> Bool that tells whether two lists of integers are equal

eql :: [Int] -> [Int] -> Bool
eql [] [] = True
eql [] _ = False
eql _ [] = False
eql (x1 : x1s) (x2 : x2s)
  | x1 == x2 = eql x1s x2s
  | otherwise = False

eql' :: [Int] -> [Int] -> Bool
eql' x y
  | length x /= length y = False
  | otherwise = and $ zipWith (==) x y -- The $ evaluates the right side expression and provides its value to the left side operand.

-- Implement a function prod :: [Int] -> Int that returns the product of a list of integers
prod :: [Int] -> Int
prod = foldl (*) 1

{-
foldl
foldl:: (a -> b -> a) -> a -> [b] -> a
-}

-- Implement a function prodEvens :: [Int] -> Int that returns the product of all even numbers of a list of integers
prodEvens :: [Int] -> Int
prodEvens = foldl (\acc x -> if even x then acc * x else acc) 1

prodEvens' :: [Int] -> Int
prodEvens' = prod Main.. filter even

-- Implement a function powersOf2 :: [Int] that generates the list of all the powers of 2
powersOf2 :: [Int]
powersOf2 = Main.map (2 ^) [0, 1 ..]

powersOf2' :: [Int]
powersOf2' = iterate (* 2) 1

-- Implement a function scalarProduct :: [Float] -> [Float] -> Float that returns the dot product of two lists of float numbers with the same size.
scalarProduct :: [Float] -> [Float] -> Float
-- scalarProduct xs ys = sum (zipWith (*) xs ys)
scalarProduct xs ys = sum $ zipWith (*) xs ys

-- Implement a function flatten :: [[Int]] -> [Int] that flattens a list of lists of integers into a list of integers
flatten :: [[Int]] -> [Int]
flatten = foldl (++) []

-- Implement a function myLength::String -> Int that returns the length of a String
myLength :: String -> Int
myLength = foldl (\acc _ -> acc + 1) 0

myLength' :: String -> Int
myLength' = sum Prelude.. Prelude.map (const 1)

-- Implement a function myReverse:: [Int] -> [Int] that reverses a list of integers.
myReverse :: [Int] -> [Int]
-- myReverse = foldl (\acc x -> x : acc) []
myReverse = foldl (flip (:)) []

-- Implement a function countIn :: [[Int]] -> Int -> [Int] that, given a list of sublists l and an element x, returns the list that tells how many times x appears
-- -- in each sublist of l
countIn :: [[Int]] -> Int -> [Int]
countIn l x = foldl (\acc sl -> acc ++ [sum $ Prelude.map (\v -> if v == x then 1 else 0) sl]) [] l

countIn' :: [[Int]] -> Int -> [Int]
countIn' l x = Prelude.map count l
  where
    count :: [Int] -> Int
    count = length Prelude.. filter (== x)
