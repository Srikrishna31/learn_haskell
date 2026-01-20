{-
    Reasoning about Haskell

    The style algebraic equational reasoning can be used in Haskell as well.
    While reasoning about Haskell, we do not just use properties of built-in operations of the language such as addition and
    multiplication, but also use the equqations from which user-defined functions are constructed.
-}
double :: Int -> Int
double x = x + x

{-
    As well as being viewed as the definition of a function, this equation can also be viewed as a property that can be used
    when reasoning about this function. In particular, as a logical property the above equation states that for any integer
    expression x, the expression double x can freely be replaced by x + x, and, conversely, that the expression x + x can freely
    be replaced by double x.
    In this manner, when reasoning about programs, function definitions can be both applied from left-to-right and unapplied
    from right-to-left.

    However, some care is required when reasoning about functions that are defined using multiple equations.
-}
isZero :: Int -> Bool
isZero 0 = True
isZero n = False

{-
    The first equation, isZero 0 = True, can freely be viewed as a logical property that can be applied in both directions. However,
    this is not the case for the second equation, isZero n = False. In particular, because the order in which the equations are written
    is significant in Haskell, an expression of the form isZero n can only be replaced by False provided that n /= 0, as in the case
    when n = 0, the first equation applies. Dually, it is only valid to unapply the equation isZero n = False and replace False by an
    expression of the form isZero n in the case when n /= 0, for the same reason.

    More generally, when a function is defined using multiple equations, the equations cannot be viewed as logical properties in
    isolation from one another, but need to be interpreted in light of the order in which patterns are matched within the equations.
    For this reason, it is preferable to define functions in a manner that does not rely on the order in which their equations are written.
-}
isZero' :: Int -> Bool
isZero' 0 = True
isZero' n | n /= 0 = False

{-
    Patterns that donot rely on the order in which they are matched are called non-overlapping. In order to simplify the process of
    reasoning about programs, it is good practice to use non-overlapping patterns whenever possible when defining functions.
-}
reverse :: [a] -> [a]
reverse [] = []
reverse (x : xs) = Main.reverse xs ++ [x]

{-
    Using this definition, we can show that reverse has no effect on singleton lists: reverse [x] = [x] for any x.

        reverse [x]
            = {list notation}
        reverse (x : [])
            = {applying reverse}
        reverse [] ++ [x]
            =  {applying reverse}
        [] ++ [x]
            = {applying ++}
        [x]

    Equational reasoing is often combined with some form of case analysis.
-}
not :: Bool -> Bool
not False = True
not True = False

{-
    Induction on Numbers

    Most interesting Haskell programs involve some form of recursion. Reasoning about such programs normally proceeds using
    the simple but powerful technique of induction.
-}
data Nat = Zero | Succ Nat deriving (Show)

inf :: Nat
inf = Succ inf

{-
    Now suppose we want to prove that some property, say p, holds for all (finite) natural numbers. Then the principle of induction
    states that it is sufficient to show that p holds for Zero, called the base case, and that p is preserved by Succ, called the
    inductive case. More precisely, in the inductive case one is required to show that if the property p holds  for any natural number
    n, called the induction hypothesis, then it also holds for Succ n (n+1).
    In the above sense, it is equivalent to think of induction principle as domino effect. If one falls, all the others also fall.
-}
add :: Nat -> Nat -> Nat
add Zero m = m
add (Succ n) m = Succ (add n m)

{-
    Let us show that the addition of natural numbers is associative: add x (add y z) = add (add x y) z
    Note that the add function is defined by pattern matching on its first argument, so it is natural to try induction on x, which
    appears twice as the first argument to add in the associativity equation, whereas y only appears once as such and z never.

        Base case:

            add Zero (add y z)
                = {applying the outer add}
            add y z
                = {unapplying add }
            add (add Zero y) z

        Inductive case:
            add (Succ x) (add y z)
                = {applying the outer add }
            Succ (add x (add y z))
                = {induction hypothesis}
            Succ (add (add x y) z)
                = {unapplying the outer add}
            add (Succ (add x y) z)
                = {unapplying the inner add}
            add (add (Succ x) y) z

       *** Note that both cases in the proof start by applying definitions, and conclude by unapplying definitions. This pattern is
        typical in proofs by induction. ***
-}
replicate :: Int -> a -> [a]
replicate 0 _ = []
replicate n x = x : Main.replicate (n - 1) x

{-
    Induction on Lists

    Suppose we want to prove that some property p holds for all lists. Then the induction principle for lists states that it is
    sufficient to show that p holds for the empty list [], the base case, and that if p holds for any list xs, then it also holds for
    x:xs for any element x, the inductive case. Of course, both the elementx and the list xs must be of the appropriate types.

        reverse (reverse xs) == xs

        Base case:

            reverse (reverse [])
                = {applying inner reverse}
            reverse []
                = {applying reverse}
            []

        Inductive case:
            reverse (reverse (x:xs))
                = {applying inner reverse}
            reverse ( reverse xs ++ [x])
                = {distributivity}
            reverse [x] ++ reverse (reverse xs)
                = {singleton list reverse}
            [x] ++ reverse (reverse xs)
            = {induction hypothesis}
            [x] ++ xs
            = {applying ++}
            x : xs

            Distribution of ++ over reverse is contravariant:

                reverse (xs ++ ys ) = reverse ys ++ reverse xs

            Base case:
                reverse ([] ++ ys)
                    = {applying ++}
                reverse ys
                    = {identity for ++}
                reverse ys ++ []
                    = {unapplying reverse}
                reverse ys ++ reverse []

            Inductive case:
                reverse ((x:xs) ++ ys)
                    = {applying ++}
                reverse (x:(xs ++ys))
                    = {applying reverse}
                reverse (xs ++ys) ++ [x]
                    = {induction hypothesis}
                (reverse ys ++ reverse xs) ++ [x]
                    = {unapplying the second reverse}
                reverse ys ++ reverse (x:xs)

        ++ is associative with [] as its identity.

        fmap is required to satisfy two equational laws:

            fmap id = id
            fmap (g.h) = fmap g . fmap h

        Following is the definition of fmap:

            fmap :: (a -> b) -> [a] -> [b]
            fmap _ [] = []
            fmap f (x:xs) = f x : fmap f xs

        By definition, two functions of the same type are equal if they always return the same results for the same arguments.

                TST: fmap id xs = id xs, by using the function id x = x  (identity function simply returns the argument it is passed).

                Base case:

                        fmap id []
                            =  {applying fmap}
                        []

                Inductive Case:
                        fmap id (x:xs)
                            = {applying fmap}
                        id x : fmap id xs
                            = {applying id}
                        x : fmap id xs
                            = {induction hypothesis}
                        x:xs

                TST: fmap (g.h) xs = (fmap g . fmap h) xs. (Applying (f.g) x = f (g x)), fmap g (fmap h xs)

                    Base case:
                        fmap (g.h) []
                            = {applying fmap}
                        []
                            = {unapplying fmap}
                        fmap g []
                            = {unapplying fmap}
                        fmap g (fmap h [])

                    Induction case
                        fmap (g.h) (x:xs)
                        = {applying fmap }
                        (g.h) x : fmap (g.h) xs
                        = {applying .}
                        g (h x) : fmap (g.h) xs
                        = {induction hypothesis}
                        g (h x) : fmap g (fmap h xs)
                        = {unapplying fmap}
                        fmap g (h x: fmap h xs)
                        = {unapplying fmap}
                        fmap g (fmap h (x:xs))
-}

{-
        Making append vanish

        Many recursive functions are naturally defined using the append operator ++ on lists, but this operator carries a
        considerable efficiency cost when iused recursively.
        First of all, it is easy to show that the number of reduction steps required to evaluate xs ++ ys is one greater than the
        length of xs, assuming for simplicity that both xs and ys are already fully evaluated. As a result, we say that ++ takes
        linear time in the length of its first argument.
        In turn, the number of steps required for reverse xs for a list of length n can be shown to be sum of integers from 1 to n+1,
        which is (n+1) (n+2) / 2 = (n^2 + 3n + 2) / 2, which is quadratic time.

        The trick is to attempt to define a more general function, which combines the behaviours of reverse and ++.

            reverse' xs ys = reverse xs ++ ys

        That is, applying reverse' to two lists should give the result of reversing the first list, appended together with the second
        list. If we can define such a function, then reverse itself can be redefined by reverse xs = reverse' xs [], using the fact that
        empty list is the identity for append.

            Base case:

                reverse' [] ys
                =   {specification of reverse'}
                reverse [] ++ ys
                =    {applying reverse}
                [] ++ ys
                =   {applying ++}
                ys

            Inductive case:
                reverse' (x:xs) ys
                = {specification of reverse'}
                reverse (x:xs) ++ ys
                = {applying reverse}
                reverse xs ++ [x] ++ ys
                = {associativity of ++}
                reverse xs ++ ([x] ++ ys)
                = {induction hypothesis}
                reverse' xs ([x] ++ ys)
                = {applying ++}
                reverse' xs (x:ys)
-}
reverse'' :: [a] -> [a] -> [a]
reverse'' [] ys = ys
reverse'' (x : xs) ys = reverse'' xs (x : ys)

reverse''' :: [a] -> [a]
reverse''' xs = reverse'' xs []

{-
    That is, the list is reversed by using an extra argument to accumulate the final result. The number of reduction steps
    required to evaluate reverse xs for a list of length  n using the new definition is simply n + 2.
-}
reverse'''' :: [a] -> [a]
reverse'''' = foldl (\xs x -> x : xs) []

data Tree = Leaf Int | Node Tree Tree deriving (Show)

flatten :: Tree -> [Int]
flatten (Leaf n) = [n]
flatten (Node l r) = flatten l ++ flatten r

{-
    TPT: flatten' t ns = flatten t ++ ns

    In order to prove that some property holds for all trees, the induction principle for the type Tree states that it is
    suuficient to show that it holds for all trees of the form Leaf n, and that if the property holds for any trees l and r, then
    it also holds for Node l r.

    Base case:
        flatten' (Leaf n) ns
        = {specification of flatten'}
        flatten (Leaf n) ++ ns
        = {applying flatten}
        [n] ++ ns
        = {applying ++}
        n : ns

    Inductive case:
        flatten' (Node l r) ns
        = {specification of flatten'}
        (flatten l ++ flatten r) ++ ns
        = {associativity of ++}
        flatten l ++ (flatten r ++ ns)
        = {induction hypothesis for l}
        flatten' l (flatten r ++ ns)
        = {induction hypothesis for r}
        flatten' l (flatten r' ns)
-}
flatten' :: Tree -> [Int] -> [Int]
flatten' (Leaf n) ns = n : ns
flatten' (Node l r) ns = flatten' l (flatten' r ns)

{-
    Compiler Correctness
-}

data Expr = Val Int | Add Expr Expr

eval :: Expr -> Int
eval (Val n) = n
eval (Add x y) = eval x + eval y

{-
    Such expressions can also be evaluated indirectly, by means of code that executes using a stack. In thiss context, a stack
    is simply a list of integers, and code comprises a list of push and add operations on the stack:
-}
type Stack = [Int]

type Code = [Op]

data Op = PUSH Int | ADD deriving (Show)

-- The meaning of such code is given by defining a function that executes a piece of code using an initial stack to give a final
-- stack:
--
exec :: Code -> Stack -> Stack
exec [] s = s
exec (PUSH n : c) s = exec c (n : s)
exec (ADD : c) (m : n : s) = exec c (n + m : s)

{-
    That is, the push operation places a new integer on the top of the stack, while add replaces the top two integers by their
    sum. Using these operations, it is now straightforward to define a function that compiles an expression into code. An integer
    value is compiled by simply pushing that value, while an addition is compiled by first compiling the two argument expressions
    x and y, and then adding the resulting two integers on the stack:
-}
comp :: Expr -> Code
comp (Val n) = [PUSH n]
comp (Add x y) = comp x ++ comp y ++ [ADD]

{-
    > let e = Add (Add (Val 2) (Val 3)) (Val 4)

    > eval e
    9

    > comp e
    [PUSH 2, PUSH 3, ADD, PUSH 4, ADD]

    > exec (comp e) []
    [9]

    Generalizing from above, the correctness of the compiler for expressions can be expressed as follows:

        exec (comp e) [] = [eval e]

    That is, compiling an expression and then executing the resulting code using an empty initial stack gives the same final stack
    as evaluating the expression and then converting the resulting integer into a singleton stack.

        exec (comp e) s = eval e : s

    Using induction for the type Expr, which is the same as induction for the type Tree, except that the names of the constructor
    are different, the compiler correctness equation can be verified as follows:

        Base case:

            exec (comp (Val n)) s
            = {applying comp}
            exec [PUSH n] s
            = {applying exec}
            n:s
            ={unapplying eval}
            eval (Val n) : s

        Inductive Case:

            exec (comp (Add x y)) s
            = {applying comp}
            exec (comp x ++ comp y ++ [ADD]) s
            = {associativity of ++}
            exec (comp x ++ (comp y ++ [ADD])) s
            = {distributivity law}
            exec (comp y ++ [ADD]) (exec (comp x)) s
            = {induction hypothesis for x}
            exec (comp y ++ [ADD]) (eval x : s)
            = {distributivity}
            exec [ADD] (exec (comp y)) (eval x :s))
            = {induction hypothesis for y}
            exec [ADD] (eval y : eval x : s)
            = {applying exec}
            (eval x + eval y) : s
            = {unapplying eval }
            eval (Add x y) : s

    The distributivity property states that executing two pieces of code appended together gives the same result as
    executing the two pieces of code in sequence:

        exec (c ++ d) s = exec d (exec c s)

        Base case:
            exec ([] ++ d) s
            = {applying ++}
            exec d s
            = {unapplying exec}
            exec d (exec [] s)

        Inductive case:
            exec ((PUSH n : c) ++ d) s
            = {applying ++}
            exec (PUSH n : (c++d)) s
            = {applying exec}
            exec  (c++ d) (n:s)
            = {induction hypothesis}
            exec d (exec c (n:s))
            = {unapplying inner exec}
            exec d (exec (PUSH n : c) s)

        Inductive case:
            exec ((ADD : c) ++ d) s
            = {applying ++}
            exec ADD : (c++d) s
            = {assume s of the form m:n:s'}
            exec ADD: (c++d) (m:n:s')
            = {applying exec}
            exec (c++d) (n+m:s')
            = {induction hypothesis}
            exec d (exec c (n+m:s'))
            = {unapplying exec}
            exec d (exec (ADD:c) (m:n:s'))

    The stack not having the assumed form in the second inductive case corresponds to a stack underflow error. In
    practice, this will never arise, because the structure of the compiler ensures that the stack will always contain at least
    two integers at the point when an add operation is performed.

    In fact, however, both the distributivity property and its consequent underflow issue can be avoided altoghether by
    applying the technique of supplying an extra list to eliminate the use of append.

        comp' e c == comp e ++ c
-}
comp' :: Expr -> Code -> Code
comp' (Val n) c = (PUSH n) : c
comp' (Add x y) c = comp' x (comp' y (ADD : c))

comp'' :: Expr -> Code
comp'' e = comp' e []

{-
        exec (comp' e c) s == exec c (eval e : s)

    That is compiling an expression and then executing the resulting code together with arbitrary additional code gives the
    same result as executing the additional code with the value of the expression on top of the original stack.

        Base case:
            exec (comp' (Val n) c) s
            = {applying comp'}
            exec ((PUSH n):c) s
            = {applying exec}
            exec c (n:s)
            = {unapplying eval}
            exec c (eval (Val n): s)

        Inductive case:
            exec (comp' (Add x y) c) s
            = {applying comp'}
            exec (comp' x comp' y (ADD:c)) s
            = {induction hypothesis for x}
            exec (comp'y (ADD:c)) (eval x : s)
            = {induction hypothesis for y}
            exec (ADD:c) (eval y : eval x : s)
            = {applying exec}
            exec c ((eval x + eval y) : s)
            = {unapplying eval}
            exec c (eval (Add x y) : s)

    Note that with s = c = [], this new compiler correctness result simplifies to exec (comp e) [] = [eval e], the original
    statement of correctness.
-}

{-
    Exercises
-}

{-
    Show that add n (Succ m) == Succ (add n m), by induction on n.
-}

{-
    Using this property, together with add n Zero = n, show that addition is commutative, add n m = add m n, by
    induction on n.
-}

{-
    Using the following definition for the library function that decides if all elements of a list satisfy a predicate:

        all p [] = True
        all p (x:xs) = p x && all p xs

    complete the proof of the correctness of replicate by showing that it produces a list with identical elements,
    all (==x) (replicate n  x), by induction on n >= 0. Hint: show that the property is always True.
-}

{-
    Using the definition

        [] ++ ys = ys
        (x:xs) ++ ys = x : (xs ++ ys)

    verify the following two properties, by induction on xs:

        xs ++ [] = xs
        xs ++ (ys ++ zs) = (xs ++ ys) ++ zs

    Hint: the proofs are similar to those for the add function.
-}

{-
    Using the above definition for ++, together with

        take 0 _ = []
        take _ [] = []
        take n (x:xs) = n: take (n-1) xs

        drop 0 xs = xs
        drop _ [] = []
        drop n (_:xs) = drop (n -1) xs

    show that take n xs ++ drop n xs = xs, by simultaneous induction on the integer >= 0 and the list xs. Hint: there
    are three cases, one for each pattern of arguments in the definitions of take and drop.
-}

{-
    Given the type declaration,

        data Tree = Leaf Int | Node Tree Tree

    show that the number of leaves in such a tree is always one greater than the number of nodes, by induction on
    Trees. Hint: start by defining functions that count the number of leaves and nodes in a tree.
-}

{-
    Verify the functor laws for the Maybe type.
    Hint: the proofs proceed by case analysis, and do not require the use of induction.
-}

{-
    Given the type and instance declarations below, verify the functor laws for the Tree type, by induction on trees

        data Tree a = Leaf a | Node (Tree a) (Tree a)

        instance Functor Tree where
            -- fmap :: (a->b) -> Tree a -> Tree b
            fmap g (Leaf x) = Leaf (g x)
            fmap g (Node l r) = Node (fmap g l) (fmap g r)
-}

{-
    Verify the applicative laws for the Maybe type.
-}

{-
    Verify the monad laws for the list type. Hint: the proofs can be completed using simple properties of list comprehension.
-}

{-
    Given the equation comp' e c == comp e ++ c, show how to construct the recursive definition comp', by induction on e.
-}
