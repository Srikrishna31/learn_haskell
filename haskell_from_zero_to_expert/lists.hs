{-
Lists have two constructors: [] and :

\* Empty List:
    [] :: [a]

\* Add ahead
    (:) :: a -> [a] -> [a]

    Lists in Haskell are simply linked lists

    Constructors [] and : work in constant time (DS sharing).

    Operator ++ returns the concatenation of two lists (time proportional to the length of the first list)

    Pattern discrimination allows to decompose lists:

-}

sum [] = 0
sum (x : xs) = x + Main.sum xs

{-
Lists and Patterns

We say that e1 matches e2 if there exists a substitution for the variable of e1 that make it the same as e2.
Note: The mechanism of matching is not the same as unification (Prolog).

Syntax in Patterns

Pattern decomposition can also be used in the case, where and let.
-}

sum' list =
  case list of
    [] -> 0
    x : xs -> x + sum' xs

sum'' [] = 0
sum'' (x : xs) = x + sum'' xs

divImod :: Int -> Int -> (Int, Int)
divImod n m
  | n < m = (0, n)
  | otherwise = (q + 1, r)
  where
    (q, r) = divImod (n - m) m

firstAndSecond :: [a] -> (a, a)
firstAndSecond list =
  let first : second : rest = list
   in (first, second)

{-
Texts

Texts (strings) in Haskell are lists of characters

The type String is a synonym of [Char]
Double quotes are syntactic sugar for defining texts.
-}

name1 :: [Char]
name1 = 'j' : 'i' : 'm' : []

name2 :: String
name2 = "jimael"

name3 :: String
name3 = "jim"

{-
Common Functions:

head: Return the first element in the list
head :: [a] -> a

last: Returns the  last element in the list
last :: [a] -> a

tail : Returns the list without its first element
tail :: [a] -> [a]

init: Returns the list without its last element
init :: [a] -> [a]

reverse: Reverses the order of elements in the list
reverse :: [a] -> [a]

length: Returns the length of the list (number of elements contained)
length: [a] -> Int

null: Indicates if the list is empty
null :: [a] -> Bool

elem: Can be used to check if a given element is present in the list.
elem :: Eq a => a -> [a] -> Bool
        a should belong to the type class Eq

(!!) : Indexing operation, returns the element at the provided index, starting from 0
(!!) :: [a] -> Int -> a

(++) : Concatenates two lists
(++):: [a] -> [a] -> [a]

maximum, minimum: Largest and smallest element of the non empty list
maximum :: Ord a => [a] -> a
minimum :: Ord a => [a] -> a

sum, product
sum :: Num a => [a] -> a
product :: Num a => [a] -> a

take: return the first n elements of a list
take :: Int -> [a] -> [a]

drop: remove the first n elements of a list
drop :: Int -> [a] -> [a]

zip: Combines two lists, in order. It returns the shorter combined list of the two.
zip :: [a] -> [b] -> [(a, b)]

repeat: Repeat an element infinitely, and return an infinite list
repeat :: a -> [a]

concat: Concatenate a list of list of xs into a list of xs.
concat:: [[a]] -> [a]
-}

-- Write a function myLast, that, given a list of elements of type a returns the last element of the list
myLast :: [a] -> a
myLast [] = error "Empty List"
myLast list =
  case list of
    [n] -> n
    _ : xs -> myLast xs

myLast' :: [a] -> a
myLast' list = head (reverse list)

myLast'' :: [a] -> a
myLast'' = head . reverse

-- . is a composition of functions in Haskell

-- Write a function myButLast that, given a list of elements of type a, returns the penultimate element of the list.
myButLast :: [a] -> a
myButLast [x, _] = x
myButLast (_ : xs) = myButLast xs

myButLast' :: [a] -> a
myButLast' = head . tail . reverse

-- Define a function dupli that duplicates the elements of a list
-- eg: dupli [1, 2, 3 ]
-- [1,1,2,2,3,3]
dupli :: [a] -> [a]
dupli [] = []
dupli (x : xs) = [x, x] ++ dupli xs

-- Define a function average :: [Inr] -> Float that, given a non-empty list of integers, returns its average.
average :: [Int] -> Float
average list = s / n
  where
    sumCount :: [Int] -> (Int, Int) -> (Int, Int)
    sumCount [] (s, n) = (s, n)
    sumCount (x : xs) (s, n) = sumCount xs (s + x, n + 1)

    (s1, n1) = sumCount list (0, 0)

    (s, n) = (fromIntegral s1 :: Float, fromIntegral n1 :: Float)

-- Define a function insertIn :: a -> [a] -> Int -> [a] that inserts an element in a given position into a list
insertIn :: a -> [a] -> Int -> [a]
insertIn a [] _ = [a]
insertIn a list n = insertInternal [] a list n
  where
    insertInternal :: [a] -> a -> [a] -> Int -> [a]
    insertInternal prev a rem n
      | n > 1 = insertInternal (head rem : prev) a (tail rem) (n - 1)
      | otherwise = reverse prev ++ [a] ++ rem

insertIn' :: a -> [a] -> Int -> [a]
insertIn' x xs 1 = x : xs
insertIn' x (y : ys) n = y : insertIn x ys (n - 1)
