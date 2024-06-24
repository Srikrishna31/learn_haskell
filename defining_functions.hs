import qualified Control.Applicative as Haskell
-- Defining functions

{-
New from old

The most straightforward way to define new functions is simply by 
combining one or more existing functions.
-}
even :: Integral a => a -> Bool
even n = n `mod` 2 == 0

splitAt :: Int -> [a] -> ([a], [a])
splitAt n xs = (take n xs, drop n xs)

recip :: Fractional a => a -> a
recip n = 1 / n


{-
Conditional Expressions
Haskell provides a range of different ways to define functions that
choose between a number of possible results.

Note that unlike in some programming languages, conditional
expressions in Haskell must always have an else branch, which avoids
the well-known dangling else problem.
-}
abs :: Int -> Int
abs n = if n >= 0 then n else -n

signum :: Int -> Int
signum n = if n < 0 then -1 else
                if n == 0 then 0 else 1



{-
Guarded Equations
As an alternate to using conditional expressions, functions can also
be defined using guarded equations, in which a sequence of logical
expressions called guards is used to choose between a sequence of 
results of the same type. If the first guard is True, then the first
result is chosen; otherwise, if the second is True, then the second
result is chosen, and so on. 
-}
absGuard :: Int -> Int
absGuard n | n >= 0 = n
           | otherwise = -n

{-
The symbol | is read as 'such that', and the guard otherwise is defined
in the standard prelude simply by otherwise = True. Ending a sequence
of guards with otherwise is not necessary, but provides a convenient
way of handling all other cases, as well as avoiding the possiblity 
that none of the guards in the sequence is True, which would otherwise
result in an error.
The main benefit of guarded expressions over conditional expressions
is that definitions with multiple guards are easier to read. 

-} 
signumGuard :: Int -> Int
signumGuard n | n < 0 = -1
              | n == 0 = 0
              | otherwise = 1



{- 
Pattern Matching

Pattern matching consists of a sequence of syntactic expressions called
patterns, and is used to choose between a sequence of results of the 
same type. If the first pattern is matched, then the first result is 
chosen; otherwise if the second is matched, then the second result is 
chosen, and so on.
-}
(&&) :: Bool -> Bool -> Bool
True && True = True
_ && _ = False

{-
Tuple Patterns
A tuple of patterns is itself a pattern, which matches any tuple of
the same arity whose components all match the corresponding patterns
in order.
-}
fst :: (a,b) -> a
fst (x,_) = x

snd :: (a,b) -> b
snd(_,y) = y

{-
List patterns
Similarly, a list of patterns is itself a pattern, which matches
any list of the same length whose elements all match the corresponding
patterns in order.
-}
test :: [Char] -> Bool
test ['a', _,_] = True
test _ = False

{-
In Haskell, lists are constructed one element at a time starting from
the empty list [] using an operator : called cons that constructs a new
list by prepending a new element to the start of an existing list.
Eg:
    [1,2,3]
    =     { list notation }
    1 : [2,3]
    =     { list notation }
    1 : (2 : [3])
    =     { list notation }
    1 : (2 : (3: []))
That is, [1,2,3] is just an abbreviation for 1:(2:(3:[])). To avoid
excess parentheses when working with such lists, the cons operator is
assumed to associate to the right. Eg: 1:2:3:[] means 1:(2:(3:[]))

As well as being used to construct lists, the cons operator can also
be used to construct patterns, which match any non-empty list whose
first and remaining elements match the corresponding patterns in order.
-}
beginsWith :: [Char] -> Bool
beginsWith ('a':_) = True
beginsWith _ = False

head :: [a] -> a
head (x:_) = x

tail :: [a] -> [a]
tail (_:xs) = xs

{-
Note that cons patterns must be parenthesised, because function
application has higher priority than all other operators in the
language. For example, the definition head x:_ = x without
parenthesis means (head x):_ = x, which is both the incorrect
meaning and an inavlid definition.
-}


{-
Lambda expressions

As an alternative to defining functions using equations, functions
can also be constructed using lambda expressions, which comprise a
pattern for each of the arguments, a body that specifies how the 
result can be calculated in terms of the arguments, but do not
give a name for the function itself. In other words lambda
expressions are nameless functions.

The symbol \ represents the Greek letter lambda, written as 
-}

add :: Int -> Int -> Int
add x y = x + y

add' :: Int -> (Int -> Int) 
add' = \x -> (\y -> x + y)

{-
Rewriting add in the second way has the benefit that the type for
the function and the manner in which it is defined now have the
same syntactic form, namely ? -> (? -> ?).

Lambda expressions are also useful when defining functions that
return functions as results by their very nature, rather than as
a consequence of currying. 
-}
const :: a -> b -> a
const x _ = x

{-
However, it is more appealing to define const in a way that makes
explicit that it returns a function as its result, by including
parentheses in the type and using a lambda expression in the 
definition itself
-}
const' :: a -> (b -> a)
const' x = \_ -> x

-- Finally lambda expressions can be used to avoid having to name
-- a function that is only referenced once in a program.
odds :: Int -> [Int]
odds n = map f [0..n-1]
         where f x = x*2 + 1

odds' :: Int -> [Int]
odds' n = map (\x -> x*2 + 1) [0..n-1]



{-
Operator sections

Functions such as + that are written between their two arguments
are called operators.
Any operator can be converted into a curried function that is 
written before its arguments by enclosing the name of the 
operator in parentheses, as in (+) 1 2.

In general, if # is an operator, then expressions of the form
(#), (x #), and (# y) for arguments x and y are called sections, 
whose meaning as functions can be formalized using lambda expressions
as follows:

(#) = \x -> (\y -> x # y)

(x #) = \y -> x # y

(# y) = \x -> x # y

Sections have three primary applications. First of all, they can be 
used to construct a number of simple but useful functions in a particularly
compact way, as shown below:

(+) is the addition function \x -> (\y -> x + y)
(1+) is the successor function \y -> 1 + y
(1/) is the reciprocation function \y -> 1/y
(*2) is the doubling function \x -> x*2
(/2) is the halving function \x -> x / 2

Secondly, sections are necessary when stating the type of operators, 
because an operator itself is not a valid expression in Haskell. 
For eg. the type of the addition operator + for integers is 
stated as follows:

(+) :: Int -> Int -> Int

Finally, sections are also necessary when using operators as
arguments to other functions.
-}
sum :: [Int] -> Int
sum = foldl (+) 0