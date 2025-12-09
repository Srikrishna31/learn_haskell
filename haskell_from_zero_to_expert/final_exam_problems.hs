import Data.List (elemIndex, sort)
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

{-
Consider a generic symbol table that converts texts (Strings) to values of type a
defined by
    type SymTab a = String -> Maybe a
The symbol table returns a `Maybe a` and not `a` in order to indicate unsuccessful searches.

The operations on the symbol table are:
empty:: SymTab a
get:: SymTab a -> String -> Mabye a
set:: SymTab a -> String -> a -> SymTab a

where empty creates an empty symbol table, get returns the value of a text to the symbol table,
(with Just if present or Nothing if not), and set returns a new symbol table defining a new value,
for a symbol (and overwriting the old value if the symbol was already in the table).

Implement these three operations on the given type (which you can't change).
-}
type SymTab a = String -> Maybe a

empty :: SymTab a
empty = const Nothing

-- empty = (\_ -> Nothing)

get :: SymTab a -> String -> Maybe a
-- get table key = table key
get table = table

set :: SymTab a -> String -> a -> SymTab a
set table key value = \a -> if key == a then Just value else table a

{-
Consider a knight on an mepty 8x8 chess board. Its position can be given with a
tuple indicating its row and column:

type Pos = (Int, Int) -- bottom left box is (1,1)

Remember that a knight moves in "L":
    1. Define a function in::Pos -> Bool that, given a position of a horse, returns
    if it is inside the board.

    2. Define a function moves :: Pos -> [Pos] that, given a position of the horse within
    the board, returns the list of positions withing the board where it can be found after a
    jump. The order of the list is not important: Test sets already sort it with luck. But you
    must write import Data.List(sort) at the beginning of the program.

    3. Define a function canGo3 :: Pos -> Pos -> Bool that, given a start position p within the
    board and a final position q, tells whether a horse can go from p to q in (exactly) three jumps.

    4. Now define canGo' :: Pos -> Pos -> Bool that does the same as canGo3 but taking advantage of
    the fact that lists are Monad instances.

Some test cases:

isIn (4,5)          True
isIn (0, 1)         False
isIn (4, 9)         False

moves (4,5)         [(2,4), (2,6), (3,3), (3,7), (5,3), (5,7), (6,4), (6,6)]
moves (1,1)         [(2,3), (3,2)]

canGo (1,1) (4,5)   True
cANGo 9
-}
type Pos = (Int, Int) -- bottom left box is (1,1)

isIn :: Pos -> Bool
isIn (x, y) = x >= 1 && x <= 8 && y >= 1 && y <= 8

moves :: Pos -> [Pos]
moves (x, y) = sort [(x1, y1) | x1 <- [x + 2, x - 2, x + 1, x - 1], y1 <- [y - 2, y - 1, y + 1, y + 2], isIn (x1, y1) && abs (x1 - x) /= abs (y1 - y)]

-- This is a wrong implementation. The function is stuck in an infinite loop.
canGo :: Pos -> Pos -> Bool
canGo p q = go p q 1
  where
    go :: Pos -> Pos -> Int -> Bool
    go p q i
      | q `elem` positions && (i < 3 || i > 3) = False
      | q `elem` positions && i == 3 = True
      | otherwise = checkPositions positions q i
      where
        positions = moves p
        checkPositions :: [Pos] -> Pos -> Int -> Bool
        checkPositions (p : ps) q i = go p q (i + 1) || checkPositions ps q i

canGo'' :: Pos -> Pos -> Bool
canGo'' p q = q `elem` destinations
  where
    destinations = concatMap moves $ concatMap moves $ moves p

canGo' :: Pos -> Pos -> Bool
canGo' p q = q `elem` (moves p >>= moves >>= moves)
