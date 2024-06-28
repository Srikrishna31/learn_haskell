import Data.Text.Internal.Fusion.Size (larger)
-- Recursive Functions

{-
Basic Concepts

In Haskell, it is permissible to define functions in terms of
themselves, in which case the functions are called recursive.
-}
fac:: Int -> Int
fac 0 = 1
fac n = n Prelude.* fac (n - 1)

{- 
The first equation states that the factorial of zero is one,
and is called a base case. The second equation states that the
factorial of any other number is given by the product of that 
number and the factorial of its predecessor, and is called a
recursive case.

Defining functions using recursion also allows properties
of those functions to be proved using the simple but powerful
technique of induction.
-}
(*) :: Int -> Int -> Int
m * 0 = 0
m * n = m + (m Main.* (n - 1))


{-
Recursion On Lists
Recursion can also be used to define functions on lists.
Recall that lists in Haskell are actually constructed one 
element at a time using the cons operator. Hence [2,3,4]
is just an abbreviation of 2:(3:(4:[]))
-}
product :: Num a => [a] -> a
product [] = 1
product (n:ns) = n Prelude.* Main.product ns

{-
The reverse of an empty list is simply the empty list, 
and the reverse of any non-empty list is given by appending
the reverse of its tail and a singleton list comprising the
head of the list.

-}
reverse :: [a] -> [a]
reverse [] = []
reverse (n:ns) = Main.reverse ns Prelude.++ [n]

{-
The recursive definition for ++ formalizes the idea that 
two lists can be appended by copying elements from the 
first list until it is exhausted, at which point the 
second list is joined on at the end.
-}
(++) :: [a] -> [a] -> [a]
[] ++ ys = ys
(x:xs) ++ ys = x : (xs Main.++ ys)


{-
Function to insert into a sorted list
-}
insert :: Ord a => a -> [a] -> [a]
insert x [] = [x]
insert x (y:ys) | x <= y = x:y:ys
                | otherwise = y : insert x ys


{-
Using insert we can now define a function that 
implements insertion sort, in which the empty 
list is already sorted, and any non-empty list is sorted
by inserting its head into the list that results from 
sorting its tail.

For exampel:
    isort [3,2,1,4]
        = {applying isort}
    insert 3 (insert 2 (insert 1 (insert 4 [])))
        = {applying insert}
    insert 3 (insert 2 (insert 1 [4]))
        = {applying insert}
    insert 3 (insert 2 [1,4])
        = {applying insert}
    insert 3 [1,2,4]
        = {applying insert}
    [1,2,3,4]
-}
isort :: Ord a => [a] -> [a]
isort [] = []
isort (x:xs) = insert x (isort xs)


{-
Multiple arguments
Functions with multiple arguments can also be
defined using recursion on more than one argument
at the same time.

Note below that two base cases are required in the
definition of zip, because either of the two arguments
lists can be empty.
-}
zip:: [a] -> [b] -> [(a,b)]
zip [] _ = []
zip _ [] = []
zip (x:xs) (y:ys) = (x,y) : Main.zip xs ys


drop :: Int -> [a] -> [a]
drop 0 xs = xs
drop _ [] = []
drop n (_:xs) = Main.drop (n - 1) xs


{-
Multiple Recursion
Functions can also be defined using multiple recursion, in 
which a function applied more than once in its own definition.
-}
fib :: Int -> Int
fib 0 = 0
fib 1 = 1
fib n = fib (n-2) + fib (n-1)


qsort :: Ord a => [a] -> [a]
qsort [] = []
qsort (x:xs) = qsort smaller Prelude.++ [x] Prelude.++ qsort larger
                where smaller = [a | a <- xs, a <= x]
                      larger = [b | b <- xs, b > x]

{-
Mutual recursion
Functions can also be defined using mutual recursion, in which
two or more functions are all defined recursively in terms of 
each other.
-}
even:: Int -> Bool
even 0 = True
even n = Main.odd (n - 1)

odd :: Int -> Bool
odd 0 = False
odd n = Main.even (n - 1)

{-
Similarly, functions that select the elements from a list at
all even and odd positions (counting from zero) can be defined as:
-}
evens :: [a] -> [a]
evens [] = []
evens (x:xs) = x : odds xs

odds :: [a] -> [a]
odds [] = []
odds (_:xs) = evens xs


{-
Advice on recursion

Step 1: define the type
Thinking about tyes is very helpful defining functions, so it is
good practice to define the type of a function prior to starting
to define the function itself.

Step 2: enumerate the cases
For most types of arguments, there are a number of standard cases
to consider. For lists, the standard cases are the empty list and
non-empty lists. For non-negative integers, the standard cases are
0 and n, for logical values they are False and True, and so on. As
with the type, we may need to refine the cases later on, but it is
useful to begin with the standard cases.

Step 3: define the simple cases
The simple cases often become base cases

Step 4: define the other cases
The other cases (regular cases) usually become the recursive cases.

Step 5: generalise and simplify
Once a function has been defined using the above process, it often
becomes clear that it can be generalized and simplified.
-}
init (x:xs) | null xs = []
            | otherwise = x : Main.init xs

{-
The above definition can also be written using pattern matching as 
follows:
-}
init' :: [a] -> [a]
init' [_] = []
init' (x:xs) = x : init' xs