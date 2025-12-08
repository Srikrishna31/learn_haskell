import Data.List (elemIndex)
import Data.Maybe (fromMaybe, isJust, isNothing)

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

{-
Write a function multEq :: Int -> Int -> [Int] that, given two nonzero positive numbers x and y,
outputs the infinitely increasing ordered list containing the numbers formed by multiplying the
same amount of x and y.
-}
multEq :: Int -> Int -> [Int]
multEq x y = scanl (*) 1 $ repeat (x * y)

multEq' :: Int -> Int -> [Int]
multEq' x y = myScanl (* (x * y)) 1
  where
    myScanl f init = init : myScanl f (f init)

multEq'' :: Int -> Int -> [Int]
multEq'' x y = iterate (* (x * y)) 1

{-
Write a function selectFirst :: [Int] -> [Int] -> [Int] -> [Int] that, given three lists
l1, l2, l3, returns the elements of l1 that appear in l2 in a position strictly smaller
than in l3. If an element appears in l2 and not in l3, it is considered to appear in
previous position.

selectFist [] [] []  -> []
selectFirst [8,4,5,6,12,1] [] [8,6,5,4,1]   -> []
selectFirst [8,4,5,6,12,1] [4,5,6,2,8,12] []    -> [8,4,5,6,12]
selectFirst [8,4,5,6,12,1] [4,5,6,2,8,12] [8,6,5,4,1]   -> [4,5,12]
-}
selectFirst :: [Int] -> [Int] -> [Int] -> [Int]
selectFirst [] _ _ = []
selectFirst _ [] _ = []
selectFirst (x : xs) ys zs
  | isNothing position_list2 = selectFirst xs ys zs
  | isNothing position_list3 = x : selectFirst xs ys zs
  | isJustTrue pos_res || isNothing pos_res = x : selectFirst xs ys zs
  | otherwise = selectFirst xs ys zs
  where
    position_list2 = elemIndex x ys
    position_list3 = elemIndex x zs
    pos_res = comparePos position_list2 position_list3

    comparePos :: Maybe Int -> Maybe Int -> Maybe Bool
    comparePos = liftA2 (<)

    isJustTrue :: Maybe Bool -> Bool
    isJustTrue = fromMaybe False

selectFirst' :: [Int] -> [Int] -> [Int] -> [Int]
selectFirst' xs ys zs =
  [ x
  | x <- xs,
    belongs x ys
      && ( not $
             belongs x zs
               || (position x ys < position x zs)
         )
  ]
  where
    belongs = elem
    position :: Int -> [Int] -> Int
    position x ys = pos x ys 0
      where
        pos :: Int -> [Int] -> Int -> Int
        pos x (y : ys) it
          | x == y = it
          | otherwise = pos x ys (it + 1)
