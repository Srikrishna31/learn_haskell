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
data Rational = Rational Integer Integer

instance Show Main.Rational where
  show (Rational x y) = show x ++ " / " ++ show y

instance Eq Main.Rational where
  Rational x1 y1 == Rational x2 y2 = x1 == x2 && y1 == y2

rational :: Integer -> Integer -> Main.Rational
rational x y = Rational (x `div` gcdx) (y `div` gcdx)
  where
    gcdx = gcd x y

numerator :: Main.Rational -> Integer
numerator (Rational x _) = x

denominator :: Main.Rational -> Integer
denominator (Rational _ y) = y

-- instance Main.Rational Show where
--   show Main.Rational (x, y) = show x ++ " / " ++ show y
