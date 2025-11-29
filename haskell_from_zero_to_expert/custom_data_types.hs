{-
In Haskell, types and typeclasses are both key concepts, but they serve very different purposes.

A type defines a set of values and how data is structured in a program. It's essentially a way of classifying values into
different kinds, such as Int, Bool, or user-defined.

Types in Haskell are used to specify what kind of values functions take as arguments and return as results.

TypeClasses

A typeclass is more like interface in other languages. It defines a set of functions that can operate on multiple types,
but it doesn't specify the data structure itself.

Instead, it defines behaviors or capabilities that a type must implement to belong to that class.

The Eq typeclass in Haskell is an excellent example to help understand what a typeclass is and how it works.
    - Eq is a typeclass in Haskell that defines equality for types.
    - Any type that is an instance of the Eq typeclass must implement the equality function == and its complementary function /=.
    - In simpler terms, if a type is an instance of Eq, it means that values of that type can be compared for equality or inequality.

    Custom Data Types

In order to create a custom data type, we use the data keyword.
For instance, the Bool type is defined in the standard library in this way:

    data Bool = True | False

data means we are defining a new data type.

The parts after the = are value constructors. They specify the different values that this type can have.
The | symbol is interpreted as "or". Therefore, we can say that the Bool type can have a value of either True or False. Both the
type name and its value constructors must be captialized.

Value constructors are nothing but functions, which take the arguments and return the type.
-}

type Radius = Float

data Shape = Circle Float Float Radius | Square Float deriving (Show)

area :: Shape -> Float
area (Circle _ _ r) = pi * r ^ 2
area (Square s) = s * s
