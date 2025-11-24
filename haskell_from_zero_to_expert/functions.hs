-- Functions
-- Functions in haskell are pure: they only return results calculated relative to their parameters.
--
-- Functions do not have side effects.
--     * they do not modify the parameters
--     * they do not modify the memory
--     * they do not modify the input/output
-- A function always returns the same result applied to the same parameters.
--
-- Function identifiers start with a lowercase.

double :: Int -> Int
double x = x + x


perimeter :: Int -> Int -> Int
perimeter width height = double(width + height)

xOr :: Bool -> Bool -> Bool
xOr a b = (a || b) && not (a && b)

factorial :: Integer -> Integer
-- factorial n = if n == 0 then 1 else n * factorial (n - 1)
factorial 0 = 1
factorial n = n * factorial (n - 1)

{-
Functions can be defined with patterns.
The evaluation of the patterns is from top to bottom and returns the result of the first matching branch.
Patterns are considered more elegant than the if-then-else and they have many more applications.
-}


nand :: Bool -> Bool -> Bool
nand True True = False
nand _ _ = True


{-
Fnctions can be defined with guards:

Guard evaluation is top-down and returns the result of the first true branch.
Pattern definitions can also have guards.

The otherwise is the same as True, but more readable.
-}

valAbs :: Int -> Int
valAbs n
  | n >= 0 = n
  | otherwise = -n


{-
Local definitions
To define local names in an expression, we can use let-in
-}

fastExp :: Integer -> Integer -> Integer
fastExp _ 0 = 1
fastExp x n =
    let y = fastExp x n_halved
        n_halved = n `div` 2
    in
        if even n
        then y * y
        else y * y * x

{-
The where allows names to be defined in more than one expression:
Indentation is important for where clause, and the indented lines define the scope of the where clause.
-}
fastExp1 :: Integer -> Integer -> Integer
fastExp1 _ 0 = 1
fastExp1 x n
    | even n = y * y
    | otherwise = y * y * x
    where
        y = fastExp1 x n_halved
        n_halved = n `div` 2


{-
Currying

All functions have a single parameter.

Functions of more than one parameter actually return a new function.

No need to pass all parameters (partial application).
-}

prod :: Integer -> Integer -> Integer
prod x y = x * y


{-
Functions solved problems
-}

-- Write a function absValue :: Int -> Int that, given an integer, returns its absolute value.
absValue :: Int -> Int
absValue n
    | n >= 0 = n
    | otherwise = -n


-- Write a function power :: Int -> Int -> Int that, given an integer x and a natural p, returns the p-th power of x, that is, x^p.
power :: Int -> Int -> Int
power x p
    | p == 0 = 1
    | otherwise = x * power x (p - 1)
