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
and :: Bool -> Bool -> Bool
True `and` True = True
_ `and` _ = False

and' :: Bool -> Bool -> Bool
True `and'` True = True
_ `and'` _ = False

and'' :: Bool -> Bool -> Bool
True `and''` b = b
False `and''` _ = False

and''' :: Bool -> Bool -> Bool
b `and'''` c | b == c = b
             | otherwise = False

and'''' :: Bool -> Bool -> Bool
b `and''''` c = if b then 
                    if c then 
                        True 
                    else 
                        False 
                else False


and''''' :: Bool -> Bool -> Bool
True `and'''''` b = if b then True else False
False `and'''''` _ = False


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


-- Exercises

{-
Using library functions, define a function halve, that splits an 
even-lengthed list into two halves.
-}
halve ::[a] -> ([a], [a])
-- halve xs = (take l xs, drop l xs)
--             where l = length xs
halve xs | Main.even (len) = (take (h) xs, drop (h) xs)
                                where len = length xs; h = len `div` 2


{-
Define a function third::[a] -> a that returns the third element in a 
list that contains at least this many elements using:
a. head and tail;
b. list indexing !!
c. pattern matching
-}
third:: [a] -> a
third xs = Prelude.head (Prelude.tail (Prelude.tail xs))

third'' :: [a] -> a
third'' xs = xs !! 2

third' :: [a] -> a
third' (_:_:x:_) = x


{-
Consider a function safetail :: [a] -> [a] that behaves in the same way
as tail except that it maps the empty list to itself rather than producing
an error. Using tail and the function null :: [a] -> Bool that decides if
a list is empty or not, define safetail using:

a. a conditional expression;

b. guarded equations;

c. pattern matching
-}
safetail :: [a] -> [a]
safetail xs = if null xs then [] else Prelude.tail xs

safetail' :: [a] -> [a]
safetail' xs | null xs = []
             | otherwise = Prelude.tail xs

safetail'' :: [a] -> [a]
safetail'' (_:xs) = xs
safetail'' _ = []


{-
In a similar way to && in section 4.4, show how the disjunction operator ||
can be defined in four different ways using pattern matching
-}
or :: Bool -> Bool -> Bool
True `or` True = True
True `or` False = True
False `or` True = True
False `or` False = False

or' :: Bool -> Bool -> Bool
False `or'` False = False
_ `or'` _ = True

or'' :: Bool -> Bool -> Bool
b `or''` c | b == c && not b = False
           | otherwise = True

or''' :: Bool -> Bool -> Bool
False `or'''` b = b
True `or'''` _ = True



{-
Show how the meaning of the following curried function definition can
be formalized in terms of lambda expressions:
-}
mult :: Int -> Int -> Int -> Int
mult x y z = x * y * z


mult':: Int -> Int -> Int -> Int
mult' = \x -> \y -> \z -> x * y * z


{-
The Luhn algorithm is used to check bank card numbers for simple
errors such as mistyping a digit, and proceeds as follows:

* consider each digit as a separate number;
* moving left, double every other number from the second last;
* subtract 9 from each number that is now greater than 9;
* add all the resulting numbers together;
* if the total is divisible by 10, the card number is valid.

Define a function luhnDouble :: Int -> Int that doubles a digit
and subtracts 9 if the result is greater than 9. For example:
    > luhnDouble 3
    6

    > luhnDouble 6
    3

Using luhnDouble and the integer remainder function mod, define
a function luhn:: Int -> Int -> Int -> Int -> Bool that decides
if a four-digit bank card number is valid. For example:
    > luhn 1 7 8 4
    True

    > luhn 4 7 8 3
    False

In the exercises for chapter 7 we will consider a more general
version of this function that accepts card numbers of any length.
-}
luhnDouble :: Int -> Int
luhnDouble b = if 2*b < 9 then 2*b else 2*b - 9

luhn :: Int -> Int -> Int -> Int -> Bool
luhn a b c d = (luhnDouble a +  b + luhnDouble c + d) `mod` 10 == 0
