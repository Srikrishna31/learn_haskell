{-
List Types
A `list` is a sequence of elements of the same type, with the elements being enclosed in square
parentheses and separated by commas. We write [T] for the type of all lists whose elements
have type T. 
-}

{-
Tuple Types
A `tuple` is a finite sequence of components of possibly different types, with the components
being enclosed in round parentheses and separated by commas. We write (T1, T2,...,Tn) for the
type of all tuples whose ith components have type Ti for any i in the range 1 to n.
-}

{- 
Function Types
A function is a mapping from arguments of one type to results of another type. We write 
T1 -> T2 for the type of all functions that map arguments of type T1 to results of type
T2.

Note that there is no restriction that functions must be total on their argument type,
in the sense that there may be some arguments for which the result is not defined.
-}

add :: (Int, Int) -> Int
add (x, y) = x + y

zeroto :: Int -> [Int]
zeroto n = [0..n]

{-
-}