-- Declaring types and classes

{-
Type declarations

The simplest way of declaring a new type is to introduce a new name for an existing
type, using the type mechanism of Haskell.
The name of a new type must begin with a capital letter. Type declarations can be
nested, in the sense that one such type can be declared in terms of another.
-}
type Pos = (Int, Int)
type Trans = Pos -> Pos

{-
Type declarations cannot be recursive. For example the following declaration is 
not allowed:
    type Tree = (Int, [Tree])

Type declarations can also be parameterized by other types. Finally, type declarations
with more than one parameter are possible too.
-}
type Pair a = (a,a)

type Assoc k v = [(k,v)]

find :: Prelude.Eq k => k -> Assoc k v -> v
find k t = head [v | (k',v) <- t, k Prelude.== k']

{-
Data declarations
A completely new type can be declared by specifying its values using the data mechanism
of Haskell.
In such declarations, the symbol | is read as or, and the new values of the type are
called constructors. As with new types themselves, the names of new constructors must
begin with a capital letter. Moreover, the same constructor name cannot be used in 
more than one type.
Note that the names given to new types and constructors have no inherent meaning to
the Haskell system.
Values of new types in Haskell can be used in precisely the same way as those of built-in
types. In particular, they can freely be passed as arguments to functions, returned as 
results from functions, stored in data structures, and used in patterns.
-}
data Move = North | South | East | West

move:: Move -> Pos -> Pos
move North (x,y) = (x, y+1)
move South (x,y) = (x, y-1)
move East (x,y) =(x+1,y)
move West (x,y) = (x-1,y)

moves :: [Move] -> Pos -> Pos
moves [] p = p
moves (m:ms) p = moves ms (move m p)

rev:: Move -> Move
rev North = South
rev South = North
rev East = West
rev West = East

{-
The constructors in a data declaration can also have arguments. For example, a type
of shapes that comprise circles with a given radius and rectangles with given 
dimensions can be declared by:
-}
data Shape = Circle Float | Rect Float Float

square:: Float -> Shape
square n = Rect n n

area :: Shape -> Float
area (Circle r) = pi * r ^ 2
area (Rect x y) = x * y

{-
Because of their use of arguments, the constructors Circle and Rect are actually
constructor functions, which produce results of type Shape from arguments of type
Float.
The difference between normal functions and constructor functions is that the 
latter have no defining equations, and exist purely for the purposes of building
pieces of data. For example, whereas the expression negate 1.0 can be evaluated
to -1.0 by applying the definition of negate, the expression Circle 1.0 is 
already fully evaluated and cannot be further simplified, because there are no
defining equations for Circle. Rather, the expression Circle 1.0 is just a piece
of data, in the same way that 1.0 itself is just data.

Not surprisingly, data declarations can also be parameterized.
-}
data Maybe a = Nothing | Just a

safediv :: Int -> Int -> Main.Maybe Int
safediv _ 0 = Main.Nothing
safediv m n = Main.Just (m `div` n)

safehead :: [a] -> Main.Maybe a
safehead [] = Main.Nothing
safehead xs = Main.Just (head xs)


{-
Newtype declarations

If a new type has a single constructor with a single argument, then it can also be
declared uisng the newtype mechanism.
-}
newtype Nat = N Int

type Natr = Int

data Natural = N' Int

{-
First of all, using newtype rather than type means that Nat and Int are different
types rather than synonyms, and hence the type system of Haskell ensures that they
cannot accidentally be mixed up in our programs. 
And secondly using newtype rather than data brings an efficiency benefit, because
newtype constructors such as N donot incur any cost when programs are evaluated, as
they are automatically removed by the compiler once type checking is completed. In
summary, using newtype helps improve type safety, without affecting performance.
-}


{-
Recursive Types
New types declared using the data and newtype mechanism can also be recursive.
-}
data Natrl = Zero | Succ Natrl

showNat :: Natrl -> String
showNat Zero = "0"
showNat (Succ k) = "Succ " ++ showNat k

instance Show Natrl where
    show = showNat


nat2int :: Natrl -> Int
nat2int Zero = 0
nat2int (Succ n) = 1 + nat2int n

int2nat :: Int -> Natrl
int2nat 0 = Zero
int2nat n = Succ (int2nat (n-1))

add :: Natrl -> Natrl -> Natrl
add m n = int2nat (nat2int m + nat2int n)

add' :: Natrl -> Natrl -> Natrl
add' Zero n = n
add' (Succ m) n = Succ (add m n)


data List a = Nil | Cons a (List a)

len :: List a -> Int
len Nil = 0
len (Cons _ xs) = 1 + len xs


data Tree a = Leaf a | Node (Tree a) a (Tree a)

t:: Tree Int
t = Node (Node (Leaf 1) 3 (Leaf 4)) 5
        (Node (Leaf 6) 7 (Leaf 9))


{-
A value occurs in a leaf if it matches the value at the leaf, and occurs in a 
node if it either matches the value at the node, occurs in the left subtree,
or occurs in the right subtree.
-}
occurs :: Prelude.Eq a => a -> Tree a -> Bool
occurs x (Leaf y) =  x Prelude.== y
occurs x (Node l y r) = x Prelude.== y || occurs x l || occurs x r


{-
Function to flatten a tree to list
-}
flatten :: Tree a -> [a]
flatten (Leaf x) = [x]
flatten (Node l x r) = flatten l ++ [x] ++ flatten r


{-
A more efficient definition of occurs
-}
occurs' :: Prelude.Ord a => a -> Tree a -> Bool
occurs' x (Leaf y) = x Prelude.== y
occurs' x (Node l y r) | x Prelude.== y = True
                       | x Prelude.< y = occurs' x l
                       | otherwise = occurs' x r

{-
As in nature, trees in computing come in many different forms. For example,
we can declare types for trees that have data only in their leaves, data only in
thier nodes, data of different types in their leaves and nodes, or have a list
of subtrees:
Which form of tree is most appropriate depends upon the situation. Note that in
the last example, there is no constructor for leaves, because a node with an empty
list of subtrees can play the role of a leaf.
-}
data Tree' a = Leaf' a | Node' (Tree' a) (Tree' a)

data Tree'' a = Leaf'' | Node'' (Tree'' a) a (Tree'' a)

data Tree''' a b = Leaf''' a | Node''' (Tree''' a b) b (Tree''' a b)

data Tree'''' a = Node'''' a [Tree'''' a]


{-
Class and instance declarations
In Haskell, a new class can be declared using the class mechanism.
-}
class Eq a where
    (==), (/=) :: a -> a -> Bool

    x /= y = not (x Main.== y)

{-
The above declaration states that for a type a to be an instance of the class Eq, it
must support equality and inequality operators of the specified types. In fact, because
a default definition has already been included for the /= operator, declaring an instance
only requires a definition for the == operator.
-}

instance Main.Eq Bool where
    False == False = True
    True == True = True
    _ == _ = False

{-
Only types that are declared using the data and newtype mechanisms can be made into
instances of classes. Note also that default definitions can be overridden in instance
declarations if desired.

Classes can also be extended to form new classes. For example, the class Ord of types 
whose values are totally ordered is declared in the standard prelude as an extension of
the class Eq as follows:
-}
class Main.Eq a => Ord a where
    (<), (<=), (>), (>=) :: a -> a -> Bool
    min, max :: a -> a -> a

    min x y | x Main.<= y = x
            | otherwise = y
    
    max x y | x Main.>= y = x
            | otherwise = y

{- 
That is, for a type to be an instance of Ord it must be an instance of Eq, and support
six additional operators. Because default definitions have already been included for
min and max, declaring an equality type (such as Bool) as an ordered type only requires
defining thye four comparison operators.
-}
instance Main.Ord Bool where
    False < True = True
    _ < _ = False

    b <= c = (b Main.< c) || (b Main.== c)
    b > c = c Main.< b
    b >= c = c Main.<= b


{-
Derived Instances
When new types are declared, it is usually appropriate to make them into instances of
a number of built-in classes. Haskell provides a simple facility for automatically 
making new types into instances of the classes Eq, Ord, Show and Read in the form of
the deriving mechanism. 
-}
data Bool' = False' | True'
    deriving (Prelude.Eq, Prelude.Ord, Prelude.Show, Read)

{-
Note that for the purposes of deriving instances of the class Ord of ordered types, 
the ordering on the constructors of a type is determined by their position in its
declaration. Hence, the above declaration for the type Bool, in which False appears 
before True, results in the ordering False < True.
In the case of constructors with arguments, the types of these arguments must also be
instances of any derived classes.
To derive Shape as an equality type requires that the type Float is also an equality
type, which is indeed the case. Similarly to derive Maybe a as an equality type requires
that the type a is also such a type, which then becomes a class constraint on this 
parameter. In the same manner as lists and tuples, values built using constructors
with arguments are ordered lexicographically.
-}


{-
Tautology checker
When simple logical propositions are always true, they are called tautology.
Consider a language of propositions built up from basic values (False, True)
and variables (A,B,...,Z) using negation (~), conjunction (^), implication
(=>), and parenthesis. 
The meaning of the logical operators can be defined using truth tables, which give
the resulting value for each combination of argument values.
The first step towards defining a function that decides if a proposition is a
tautology is to declare a type for propositions, with one constructor for each
of the five possible formst that a proposition can have:
The first step towards defining a function that decides if a proposition is a 
tautology is to declare a type for propositions, with one constructor for each
of the five possible forms that a proposition can have:
-}
data Prop = Const Bool
          | Var Char
          | Not Prop
          | And Prop Prop
          | Imply Prop Prop
          | Or Prop Prop
          | Equivalence Prop Prop


p1 :: Prop
p1 = And (Var 'A') (Not (Var 'A'))

p2 :: Prop
p2 = Imply (And (Var 'A') (Var 'B')) (Var 'A')

p3::Prop
p3 = Imply (Var 'A') (And (Var 'A') (Var 'B'))

p4 :: Prop
p4 = Imply (And (Var 'A') (Imply (Var 'A') (Var 'B'))) (Var 'B')

p5:: Prop
p5 = Or (Var 'A') (Not (Var 'A'))

p6:: Prop
p6 = Equivalence (And (Var 'A') (Not (Var 'A'))) (Const False)

type Subst = Assoc Char Bool

-- A function that evaluates a proposition given a substitution for its
-- variables can now be defined by pattern matching on the five possible
-- forms that the propositions can have:
eval :: Subst -> Prop -> Bool
eval _ (Const b) = b
eval s (Var x) = find x s
eval s (Not p) = not (eval s p)
eval s (And p q) = eval s p && eval s q
eval s (Imply p q) = eval s p Prelude.<= eval s q
eval s (Or p q) = eval s p || eval s q
eval s (Equivalence p q) = eval s p Prelude.== eval s q


{-
To decide if a proposition is a tautology, we will consider all possible
substitutions for the variables that it contains. First of all, we define
a function that returns a list of all the variables in a proposition:
-}
vars :: Prop -> [Char]
vars (Const _) = []
vars (Var x) = [x]
vars (Not p) = vars p
vars (And p q) = vars p ++ vars q
vars (Imply p q) = vars p ++ vars q
vars (Or p q) = vars p ++ vars q
vars (Equivalence p q) = vars p ++ vars q

{-
The key to generating substitutions is producing lists of logical values of a 
given length. Hence we seek to define a function bools:: Int -> [[Bool]] which,
for example, will return all eight lists of three logical values
-}
int2bin :: Int -> [Int]
int2bin 0 = []
int2bin n = n `mod` 2 : int2bin (n `div` 2)

bools :: Int -> [[Bool]]
bools n = map (reverse . map conv . make n . int2bin) range
            where
                range = [0..(2^n) - 1]
                make n bs = take n (bs ++ repeat 0)
                conv 0 = False
                conv 1 = True


{-
There is however a simpler way to define bools, which can be revealed by thinking
about the structure of the resulting lists. For example, we can observe that bools 3
contains two copies of bools 2, the first preceded by False in each case, and the 
second preceded by True in each case.
This observation leads to a recursive definition for bools. In the base case, bools 0,
we return all lists of zero logical values, of which the empty list is the only one.
In the recursive case, bools n, we take two copies of the lists produced by bools (n-1),
place False in front of each list in the first copy, True in front of each list in the
second, and append the results:
-}
bools' :: Int -> [[Bool]]
bools' 0 = [[]]
bools' n = map (False:) bss ++ map (True:) bss
            where bss = bools (n - 1)


{-
substs function generates all possible substitutions for a proposition by extracting
its variables, removing duplicates from the list, generating all possible lists of 
logical values for this many variables, and then zipping the list of variables with
each of the resulting lists:
-}
rmdups :: Prelude.Eq a => [a] -> [a]
rmdups [] = []
rmdups (x:xs) = x : filter (Prelude./=x) (rmdups xs)


substs:: Prop -> [Subst]
substs p = map (zip vs) (bools (length vs))
            where vs = rmdups (vars p)


isTaut :: Prop -> Bool
isTaut p = and [eval s p | s <- substs p]


{-
Abstract Machine
Here we define an abstract machine for expressions, which specifies the step-by-step
process of their evaluation.
For this we first define a type of control stack for the abstract machine, which comprises
a list of operations to be performed by the machine after the current evaluation has
been completed.
-}
data Expr = Val Int | Add Expr Expr | Mul Expr Expr

type Cont = [Op]

data Op = EVAL Expr | ADD Int | MUL Int | EVALMUL Expr

eval' :: Expr -> Cont -> Int
eval' (Val n) c = exec c n 
eval' (Add x y) c = eval' x (EVAL y: c)
eval' (Mul x y) c = eval' x (EVALMUL y: c)

value:: Expr -> Int
value (Val n) = n
value (Add x y) = value x + value y
value (Mul x y) = value y * value y

exec:: Cont -> Int -> Int
exec [] n = n
exec (EVAL y: c) n = eval' y (ADD n : c)
exec (EVALMUL y: c) n = eval' y (MUL n : c)
exec (ADD n : c) m = exec c (n+m)
exec (MUL n : c) m = exec c (n * m)

value' :: Expr -> Int
value' e = eval' e []

{-
The fact that the abstract machine uses two mutually recursive functions eval and exec,
reflects the fact that it has two modes of operation, depending upon whether it is being
driven by the structure of the expression or the control stack.
-}


-- Exercises

{-
In a similar manner to the function add, define a recursive multiplication function 
mult:: Nat -> Nat -> Nat for the recursive type of natural numbers:
Hint: make use of add in your definition
-}
mult :: Natrl -> Natrl -> Natrl
mult _ Zero = Zero
mult Zero _ = Zero
mult m n | nat2int n Prelude.== 1 = m
         | nat2int m Prelude.== 1 = n
         | otherwise = mult (add m m)  (int2nat (nat2int n - 1))


{-
Although not included in appendix B, the standard prelude defines
    data Ordering = LT | EQ | GT
together with a function
    compare :: Ord a => a -> a -> Ordering
that decides if one value in an ordered type is less than (LT), equal to (EQ), or 
greater than (GT) another value. Using this function, redefine the function
occurs::Ord a => a -> Tree a -> Bool for search trees. Why is this new definition
more efficient than the original version?

Copied from the solution provided. 
Of course as explained, the comparison happens once which could lead to efficient
search, since the comparison function returns Haskell equivalent of Enums.
-}
occurs'' :: Prelude.Ord a => a -> Tree a -> Bool
occurs'' x (Leaf y) = x Prelude.== y
occurs'' x (Node l y r) = case compare x y of 
                            LT -> occurs x l
                            EQ -> True
                            GT -> occurs x r


{-
Consider the following type of binary trees:
    data Tree a = Leaf a | Node (Tree a) (Tree a)

Let us say that such a tree is balanced if the number of leaves in the left and
right subtree of every node differs by at most one, with leaves themselves being
trivially balanced. Define a function balanced:: Tree a -> Bool that decides if
a binary tree is balanced or not.

Hint: first define a function that returns the number of leaves in a tree.
-}

numLeaves :: Tree' a -> Int
numLeaves (Leaf' _) = 1
numLeaves (Node' l r) = numLeaves l + numLeaves r

balanced :: Prelude.Ord a => Tree' a -> Bool
balanced (Leaf' _) = True
balanced (Node' l r) = abs (numLeaves l - numLeaves r) Prelude.<= 1
                        && balanced l && balanced r


{-
Define a function balance :: [a] -> Tree a that converts a non-empty list into
a balanced tree. 
Hint: first define a function that splits a list into two halves whose length 
differs by at most one.
-}
split :: [a] -> ([a], [a])
split as = splitAt n as
            where n = length as `div` 2

balance ::[a] -> Tree' a 
balance [a] = Leaf' a
balance as = Node' (balance ls)  (balance rs)
                where (ls, rs) = split as

{-
Given the type declaration 
data Expr = Val Int | Add Expr Expr

define a higher-order function
    folde:: (Int -> a) -> (a->a->a) -> Expr -> a
such that folde f g replaces each Val constructor in an expression by the
function f, and each Add constructor by the function g.
-}
folde::(Int -> a) -> (a->a->a) -> Expr -> a
folde f _ (Val a) = f a
folde f g (Add e1 e2) = g (folde f g e1) (folde f g e2)


{-
Using folde, define a function eval::Expr -> Int that evaluates an expression
to an integer value, and a function size::Expr -> Int that calculates the
number of values in an expression
-}
eval'' :: Expr -> Int
-- eval'' = folde id (\v1 -> \v2 -> v1 + v2)
eval'' = folde id (+)

size'' :: Expr -> Int
size'' = folde (const 1) (+)


{-
Complete the following instance declarations:
-}
instance Main.Eq a => Main.Eq (Main.Maybe a) where
    (==)::Main.Maybe a -> Main.Maybe a -> Bool
    (Main.Just a1) == (Main.Just a2) = a1 Main.== a2
    _ == _ = False


instance Main.Eq a => Main.Eq [a] where
    (==)::[a] -> [a] -> Bool
    [] == [] = True
    [] == _ = False
    _ == [] = False
    (x:xs) == (y:ys) = (x Main.== y) && (xs Main.== ys)


{-
Extend the tautology checker to support the use of logical disjunction (V) and 
equivalence (<=>) in propositions

-- Modified the definitions of tautology related functions above.
-}

{-
Extend the abstract machine to support the use of multiplication.
-}
-- Modified the definitions of abstract machine related functions above.
