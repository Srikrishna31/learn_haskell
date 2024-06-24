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
Curried Functions

Functions with multiple arguments can also be handled in another way as shown below:
The type states that add' is a function that takes an argument of type Int, and returns
a result that is a function of type `Int -> Int`. The definition itself states that 
add' takes an integer x followed by an integer y, and returns the result x+y.
More precisely, add' takes an integer x and returns a function, which in turn takes
an integer y and returns the result x + y.
-}
add' :: Int -> (Int -> Int)
add' x y = x + y

mult :: Int -> (Int -> (Int -> Int))
mult x y z = x*y*z

{- 
Functions such as add' and mult that take thier arguments one at a time are called
`curried functions`.
To avoid excess parentheses when working with curried functions, two simple 
conventions are adopted. First of all, the function arrow -> in types is assumed
to associate to the right. For example, the type 

Int -> Int -> Int -> Int
means
Int -> (Int -> (Int -> Int))
Consequently, function application which is deonoted silently using spacing, is
assumed to associate to the left. For example, the application

mult x y z

means

((mult x) y) z

Unless tupling is explicitly required, all functions in Haskell with multiple
arguments are normally defined as curried functions, and the two conventions
above are used to reduce the number of parentheses that are required.
-}

{- 
Polymorphic Types

The idea that length can be applied to lists whose eleemnts have any type is
made precise in its type by the inclusion of a `type variable`. Type variables
must begin with a lower-case letter, and are usually simply named a, b, c
and so on. A type that contains one or more type variables is called 
`polymorphic`.
The type of a polymorphic function often gives a strong indication about the 
function's behavior, although the type on its own doesn't capture the
precise manner in which the behavior has to be achieved.

Eg: 
fst :: (a,b) -> a

head :: [a] -> a

take :: Int -> [a] -> [a]

zip :: [a] -> [b] -> [(a,b)]

id :: a -> a
-}

{- 
Overloaded types

The idea that + can be applied to numbers of any numeric type is made precise
in its type by the inclusion of a class constraint. Class constraints are
written in the form C a, where C is the name of a class and a is a type variable:

(+) :: Num a => a -> a -> a

That is, for any type a that is an instance of the class Num of numeric types, the
function (+) has type a -> a -> a. (Parenthesising an operator converts it into
a curried function)

A type that contains one or more class constraints is called overloaded, as is
an expression with such a type.
-}

{- 
    Basic Classes
A type is a collection of related vaues. A class is a collection of types that
support certain overloaded operations called methods.

Eq - Equality Types
This class contains types whose values can be compared for equality and inequlity
using the following two methods:

(==) :: a -> a -> Bool

(/=) :: a -> a -> Bool

All the basic types Bool, Char, String, Int, Integer, Float and Double are 
instances of the Eq class, as are list and tuple types, provided that their
element and component types are instances.
Note that function types are not in general instances of the Eq class, because
it is not feasible in general to compare two functions for equality.


Ord - ordered types

This class contains types that are instances of the equality class Eq, but in
addition whose values are totally (linearly) ordered, and as such can be 
compared and processed using the following six methods:

(<) :: a -> a -> Bool

(<=) :: a -> a -> Bool

(>) :: a -> a -> Bool

(>=) :: a -> a -> Bool

min :: a -> a -> a 

max :: a -> a -> a

All the basic types Bool, Char, String, Int, Integer, Float and Double
are instances of the Ord class, as are list and tuple types, provided
that the element and component types are instances.


Show - showable types

This class contains types whose values can be converted into strings
of characters using the following method:

show :: a -> String

All the basic types Bool, Char, String, Int, Integer, Float and Double
are instances of the Ord class, as are list and tuple types, provided
that the element and component types are instances.


Read - readable types

This class is dual to Show, and contains types whose values can be
converted from strings of characters using the following method:

read :: String -> a

All the basic types Bool, Char, String, Int, Integer, Float and Double
are instances of the Ord class, as are list and tuple types, provided
that the element and component types are instances.


Num - numeric types

This class contains types whose values are numeric, and as such can be
processed using the following six methods:

(+) :: a -> a -> a
(-) :: a -> a -> a
(*) :: a -> a -> a
negate :: a -> a
abs :: a -> a
signum :: a -> a

Integral - integral types

This class contains types that are instances of the numeric class Num,
but in addition whose values are integers, and as such support the 
methods of integer and integer remainder:

div :: a -> a -> a

mod :: a -> a -> a


Fractional - fractional types

This class contains types that are instances of the numeric class
Num, but in addition whose values are non-integral, and as such 
support the methods of fractional division and fractional reciprocation:

(/) :: a -> a -> a

recip :: a -> a
-}


{- Exercises

Write down definitions that have the following types; it doesnot matter what the 
definitions actually do as long as they are type correct.

bools:: [Bool]
-}
bools = [True, False, True]

-- nums :: [[Int]]
nums::[[Int]]
nums = [[1,2,3], [4,5,6], [7,8,9]]


-- using add as in question is giving an error, so name the function as addCustom
addCustom :: Int -> Int -> Int -> Int
addCustom a b c = a + b + c


apply :: (a -> b) -> a -> b
apply f a = f(a)


{- 
Whate are the types of the following functions?
Hint: take care to include the necessary class constraints in the types if the 
    functions are defined using overload operators.
-}

-- second:: [a] -> a
second xs = head (tail xs)

-- swap :: (a, b) -> (b, a)
swap (x, y) = (y,x)

-- pair :: a -> b -> (a,b)
pair x y = (x,y)

-- double :: Num a => a -> a
double x = x * 2

-- palindrome :: [a] -> Bool
palindrome xs = reverse xs == xs

-- twice :: (a -> b) -> a -> b
twice f x = f (f x)


{-
Why is it not feasible in general for function types to be instances of the Eq
class? When is it feasible?

In general, functions are equal when they produce equal results for equal arguments.
Since the argument combinations can explode, in general it is not feasible to 
check them thoroughly for all combinations.
On the other hand, if the argument types have few values e.g. Bool or Enumerations,
    then it might be feasible to check for equality of functions.
-}