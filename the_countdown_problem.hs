-- The Countdown Problem

{-
Given a sequence of numbers and a target number, attempt to construct an expression
whose value is the target, by combining one or more numbers from the sequence using
addition, subtraction, multiplication, division and parentheses.

Each number in the sequence can only be used at most once in the expression, and all
of the numbers involved, including intermediate values, must be positive natural
numbers (1,2,3,...). In particular, the use of negative numbers, zero, and proper
fractions such as 2/3, is not permitted.
-}

data Op = Add | Sub | Mul | Div

instance Show Op where
    show Add = "+"
    show Sub = "-"
    show Mul = "*"
    show Div = "/"

{-
We define a function valid that decides if the application of an operator to two
positive naturals gives another positive natural, and a function apply that
actually performs such a valid application:
-}
-- valid :: Op -> Int -> Int -> Bool
-- valid Add _ _ = True
-- valid Sub x y = x > y
-- valid Mul _ _ = True
-- valid Div x y = x `mod` y == 0

{-
The above definition is modified to exploit the commutativity of addition and 
multiplication simply by requiring that their arguments are in numeric order
(x <= y), and the identity properties of multiplication and division simply by
requiring that the appropriate arguments are non-unitary (/= 1)
-}
valid :: Op -> Int -> Int -> Bool
valid Add x y = x <= y
valid Sub x y = x > y
valid Mul x y = x /= 1 && y /= 1 && x <= y
valid Div x y = y /= 1 && x `mod` y == 0


apply :: Op -> Int -> Int -> Int
apply Add x y = x + y
apply Sub x y = x - y
apply Mul x y = x * y
apply Div x y = x `div` y

{-
Numeric Expressions
Declare a type for numeric expressions, which can either be an integer value or
the application of an operator to two argument expressions, together with a 
pretty-printer for expressions:
-}
data Expr = Val Int | App Op Expr Expr

instance Show Expr where
    show (Val n) = show n
    show (App o l r) = brak l ++ show o ++ brak r 
                        where 
                            brak (Val n) = show n
                            brak e = "(" ++ show e ++ ")"

{-
Using this type, we define a function that returns the list of values in an 
expression, and a function eval that returns the overall value of an expression,
provided that this value is a positive natural number:
-}
values :: Expr -> [Int]
values (Val n) = [n]
values (App o l r) = values l ++ values r

eval :: Expr -> [Int]
eval (Val n) = [n | n > 0]
eval (App o l r) = [apply o x y | x <- eval l,
                                  y <- eval r,
                                  valid o x y]


{-
Combinatorial functions
Define a number of useful combinatorial functions that return all possible lists
that satisfy certain properties. The function subs returns all subsequences of a 
list, which are given by all possible combinations of excluding or including each
element of the list, interleave returns all possible ways of inserting a new 
element into a list, and finally, perms returns all permutations of a list, which
are given by all possible reorderings of the elements:
-}
subs :: [a] -> [[a]]
subs [] = [[]]
subs (x:xs) = yss ++ map (x:) yss
                where yss = subs xs

interleave :: a -> [a] -> [[a]]
interleave x [] = [[x]]
interleave x (y:ys) = (x:y:ys) : map (y:) (interleave x ys)


perms :: [a] -> [[a]]
perms [] = [[]]
perms (x:xs) = concat (map (interleave x) (perms xs))

{-
In turn, a function that returns all choices from a list, which are given by all
possible ways of selecting zero or more elements in any order, can then be defined
simply by considering all permutations of all subsequences:
-}
choices :: [a] -> [[a]]
choices = concat . map perms . subs


{-
Formalising the problem
Finally, we can now define a function solution that formalises what it means to 
solve an instance of the countdown problem:
That is, an expression is a solution for a given list of numbers and a target if
the list of values in the expression is chosen from the list of numbers, and the
expression successfully evaluates to give the target.
-}
solution :: Expr -> [Int] -> Int -> Bool
solution e ns n = elem (values e) (choices ns) && eval e == [n]

e1 :: Expr
e1 = App Mul (App Add (Val 1) (Val 50)) (App Sub (Val 25) (Val 10))

{-
Brute Force solution
The first approach to solving the countdown problem is by brute force, using the
idea of generating all possible expressions over the given list of numbers.

Define a function split that returns all possible ways of splitting a list into
two non-empty lists that append to give the original list:
-}
split :: [a] -> [([a], [a])]
split [] = []
split [_] = []
split (x:xs) = ([x], xs) : [(x:ls, rs) | (ls, rs) <- split xs]

{-
Using split we can then define the key function exprs, which returns all possible
expressions whose list of values is precisely a given list:
-}
exprs :: [Int] -> [Expr]
exprs [] = []
exprs [n] = [Val n]
exprs ns = [e | (ls, rs) <- split ns, 
                l <- exprs ls,
                r <- exprs rs,
                e <- combine l r]

{-
That is, for the empty list of numbers there are no possible expressions, while
for a single number there is a single expression comprising that number. Otherwise,
for a list of two or more numbers we first produce all splittings of the list, then
recursively calculate all possible expressions for each of these lists, and, finally
combine each pair of expressions using each of the four numeric operators, using
an auxiliary funciton that is defined as follows:
-}
combine :: Expr -> Expr -> [Expr]
combine l r = [App o l r | o <- ops]

ops :: [Op]
ops = [Add, Sub, Mul, Div]

{-
In conclusion, we can now define a function solutions that returns all possible
expressions that solve an instance of the countdown problem, by first generating
all expressions over each choice from the given list of numbers, and then
selecting those expressions that successfully evaluate to give the target:
-}
solutions :: [Int] -> Int -> [Expr]
solutions ns n =
    [e | ns' <- choices ns, e <- exprs ns', eval e == [n]]


type Result = (Expr, Int)

{-
The function solutions generates all possible expressions over the given numbers,
but in practice many of these expressions will fail to evaluate, due to the fact
that subtraction and division are not always valid operations for positive naturals.

Based on this observation, the next approach is to combine the generation of 
expressions with their evaluation, such that both tasks are performed simultaneously.
In this way, expressions that faile to evaluate are rejected at an earlier stage,
and, more importantly, are not used to generate further expressions that will fail
to evaluate.
-}
results :: [Int] -> [Result]
results [] = []
results [n] = [(Val n, n) | n > 0]
results ns = [res | (ls, rs) <- split ns,
                    lx <- results ls,
                    ry <- results rs,
                    res <- combine' lx ry]


{-

That is, for the empty list there are no possible results, while for a single number
there is a single result formed from that number, provided that the number itself
is a positive natural number. Otherwise, for two or more numbers we first produce
all splittings of the list, then recursively calculate all possible results for each
of these lists, and, finally, combine each pair of results using each of the four
numeric operators that are valid, by means of the following auxiliary function:
-}
combine' :: Result -> Result -> [Result]
combine' (l,x) (r,y) = 
    [(App o l r, apply o x y) | o <- ops, valid o x y]


solutions' :: [Int] -> Int -> [Expr]
solutions' ns n = 
    [e | ns' <- choices ns, (e,m) <- results ns', m == n]

main:: IO ()
-- main = print (solutions [1,3,7,10,25,50] 765)
main = print (solutions' [1,3,7,10,25,50] 765)


{-
Exploiting algebraic properties
The function solutions' generates all possible expressions over the given numbers
whose evaluation is successful, but in practice many of these expressions will be
essentially the same, due to the fact that the numeric operators have algebraic
properties.
Based upon this observation, the final approach to solving the countdown problem
is to improve our second program by exploiting such algebraic properties to reduce
the number of generated expressions:

    x + y = y + x
    x * y = y * x
    x * 1 = x
    1 * y = y
    x / 1 = x
-}