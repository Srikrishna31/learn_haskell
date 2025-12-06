{-
Define a function fsmap:: a -> [a -> a] -> a which, given an element x of type a and a list fs of functions of type a -> a,
causes fsmap x fs to return the application (from left right) of all functions from fs to x
-}
fsmap :: a -> [a -> a] -> a
fsmap = foldl (\acc f -> f acc)

fsmap' :: a -> [a -> a] -> a
fsmap' a [] = a
fsmap' a (f : fs) = fsmap' (f a) fs

{-
Define a Rational type to manipulate positive rational numbers with operations by:
    1. Construct a rational through a natural numerator and denominator
    2. Obtain the numerator of its simplified form
    3. Obtain the denominator of its simplified form
    Also, make Rational a member of class Eq and Show, making rationals display in the form x / y.
-}
-- data  Rational = Numerator (Int, Int) deriving (Show, Eq)

-- instance Main.Rational Show where
--   show Main.Rational (x, y) = show x ++ " / " ++ show y
