import Data.Char (digitToInt, isDigit)

{-
The idea of mapping a function over each element of a data structure isn't specific to the type of lists, but can be
abstracted further to a wide range of parameterized types. The class of types that support such a mapping function
are called `functors`

    class Functor f where
        fmap :: (a -> b) -> f a -> f b

The intuition is that fmap takes a function of type a -> b and a structure of type f a whose elements have type a, and
applies the function to each such element to give a structure of type f b whose elements now have type b.
-}
data Tree a = Leaf a | Node (Tree a) (Tree a) deriving (Show)

instance Functor Tree where
  -- fmap :: (a -> b) -> Tree a -> Tree b
  fmap g (Leaf x) = Leaf (g x)
  fmap g (Node l r) = Node (fmap g l) (fmap g r)

{-
  instance Functor IO where
      -- fmap :: (a -> b) IO a -> IO b
      fmap g mx = do {x <- mx; return (g x)}

Following are the two benefits of using functors:
    * Firstly, the function `fmap` can be used to process the elements of any structure that is functorial. That is, we can use
    the same name for functions that are essentially the same, rather than having to invent a separate name for each
    instance.
    * Secondly, fmap can be uniformly generalized to any functorial type.
-}

{-
    Functor Laws
In addition to providing a function fmap of the specified type, functors are also required to satisfy two equational laws:

        1. fmap id = id

        The first equation states that fmap preserves the identity function, in the sense that applying fmap to this function
        returns the same function as the result. Note however that the two occurrences of id in this equation have different
        types: on the left-hand side id has type a -> a and hence fmap id has type f a -> f a, which means that the id on
        right-hand side must also have type f a -> f a in order for the equation to be well-typed.

        2. fmap (g.h) = fmap g . fmap h
        The second equation states that fmap also preserves function composition, in the sense that applying fmap to the
        composition of two functions gives the same result as applying fmap to the two functions separately and then
        composing. In order for the compositions to be well-typed, the component functions g and h must have types
        b -> c and a -> b.

In combination with the polymorphic type for fmap, the functor laws ensure that fmap does indeed perform a mapping
operation. In the case of lists, for instance, they ensure that the structure of the argument list is preserved by fmap, in
the sense that elements are not added, removed or re-arranged.

-}

{-
        Applicatives

Functors abstract the idea of mapping a function over each element of a structure. One way to think of Applicatives is to generalize
the idea to allow functions with any number of arguments to be mapped, rather than being restricted to functions with a single
argument. Let's say we wish to have the following hierarchy of fmap functions with the following types:

        fmap0 :: a -> f a

        fmap1 :: (a -> b) -> fa -> fb

        fmap2 :: (a ->  b -> c) -> f a -> f b -> f c

        fmap3 :: (a -> b -> c -> d) -> f a -> f b -> f c -> f d

        .
        .
        .

Note that fmap1 is just another name for fmap, and fmap0 is the degenerate case when the function being mapped has no
arguments. One approach would be to declare a special version of the functor class for each case: Functor0, Functor1,
Functor2 and so on.
However, this is unsatisfactory in many ways.
\* Firstly, we would have to manually declare each version of the Functor class even if they all follow a similar pattern.
\* Secondly, it is not clear how many such classes we should declare, as there are infinitely many but we can only declare a
finite number.
\* And finally, we haven't used the capabilities of currying provided by the language to avoid defining explicitly.
Infact, using the idea of currying, it turns out that a version of fmap for function with any desired number of arguments can
be constructed in terms of two basic functions with the following types:

        pure ::  a -> f a

        (<*>) ::  f (a -> b) -> f a -> f b

That is, pure converts a value of type a into a structure of type f a, while <*> is a generalized form of function application
for which the argument function, the argument value, and the result value are all contained in f structures. As with normal
function application, the <*> operator is written between its two arguments and is assumed to associate to the left.

A typeical use of pure and <*> has the following form:

    pure g <*> x1 <*> x2 <*> ... <*> xn

Such expressions are said to be in applicative style, because of the similarity to normal function application notation
g x1 x2 ... xn. In both cases, g is a curried function that takes n arguments of type a1 ... an and produces a result of type
b. However, in applicative style, each argument xi has type f ai rather than just ai, and the overall result has type f b rather
than b.

        fmap0 :: a -> f a
        fmap0 = pure

        fmap1 :: f (a -> b) -> f a -> f b
        fmap1 g x = pure g <*> x

        fmap2 :: f (a -> b -> c) -> f a -> f b -> f c
        fmap2 g x y = pure g <*> x <*> y

        fmap3 :: f (a -> b -> c -> d) -> f a -> f b -> f c
        fmap3 g x y z = pure g <*> x <*> y <*> z

        .
        .
        .

The class of functors that support `pure`  and `<*>` functions are called applicative functors, or applicatives for short.

    class Functor f => Applicative f where
        pure :: a -> f a

        (<*>) :: f (a -> b) -> f a -> f b

    Examples:

        instance Applicative Maybe where
            -- pure :: a -> Maybe a
            pure a = Just

            -- (<*>) :: Maybe (a -> b) -> Maybe a -> Maybe b
            Nothing  <*> _ = _
           (Just g) <*> mx = fmap g mx

    That is, the function `pure` transforms a value into a successful result, while the operator `<*>` applies a function
    that may fail, to an argument that may fail, to produce a result that may fail.
    In this manner, the applicative style for `Maybe` supports a form of exceptional programming in which we can
    apply pure functions to arguments that may fail without the need to manage the propagation of failure ourselves,
    as this is taken care of automatically by the applicative machinery.

        instance Applicative List where
            --pure :: a -> List a
            pure a = [a]

            -- (<*>) :: List (a -> b) -> List a -> List b
            [] <*> _ = []
            _ <*> [] = []
            (f:fs) <*> xs = fmap f xs ++ fs <*> xs

            -- (<*>) :: List ( a -> b)  -> List a -> List b
            fs <*> xs = [f x | f <- fs, x <- gs]

    That is, `pure` transforms a value into a singleton list, while `<*>` takes a list of functions and a list of arguments, and
    applies each function to each argument in turn, returning all the results in a list.

    The key is to view the type [a] as a generalization of Maybe a that permits multiple results in the case of success. More
    precisely, we can think of the empty list as representing failure, and a non-empty list as representing all the possible
    ways in which a result may succeed.
-}
prods :: [Int] -> [Int] -> [Int]
prods xs ys = [x * y | x <- xs, y <- ys]

prods' :: [Int] -> [Int] -> [Int]
prods' xs ys = pure (*) <*> xs <*> ys

{-
In summary, the applicative style for lists supports a form of non-deterministic programming in which we can apply pure
functions to multi-valued arguments without the need to manage the selection of values or the propagation of failure, as
this is taken care of by the applicative machinery.

        instance Applicative IO where
        -- pure :: a -> IO a
            pure = return

        -- (<*>) :: IO (a -> b) -> IO a -> IO b
        iof <*> iox = do {f <- iof; x <- iox; return f x}

In this case, pure is given by the return function for the IO type, and <*> applies an impure function to an impure argument
to give an impure result.
-}
getChars :: Int -> IO String
getChars 0 = return []
getChars n = pure (:) <*> getChar <*> getChars (n - 1)

{-
    More generally, the applicative style for IO supports a form of interactive programming in which we can apply pure functions
    to impure arguments without the need to manage the sequencing of actions or the extraction of result values, as this is
    taken care of automatically by the applicative machinery.

        Effectful programming

    One way to view applicatives is to generalize the idea of mapping to functions with multiple arguments. Another way to view
    it would be to consider that applicatives concern programming with effects.
    The arguments are no longer just plain values but may also have effects, such as the possibility of failure, having many ways to
    succeed, or performing input/output actions. In this manner, applicative functors can also be viewed as abstracting the idea of
    applying pure functions to effectful arguments, with the precise form of effects that are permitted depending on the nature
    of the underlying functor.
    In addition to providing a uniform approach to a form of effectful programming, using applicatives also has the important
    benefit that we can define generic functions that can be used with any applicative functor.

-}
sequenceA :: (Applicative f) => [f a] -> f [a]
sequenceA [] = pure []
sequenceA (x : xs) = pure (:) <*> x <*> Main.sequenceA xs

-- foldr (\x -> (<*>) (pure (:) <*> x)) (pure []) xs

getChars' :: Int -> IO String
getChars' n = Main.sequenceA (replicate n getChar)

{-
    The `$` operator in haskell is a function application operator with low precedence:
        ($) :: (a -> b) -> a -> b

        * $ is right-associative and has precedence 0 (lowest possible)
        * It's used to avoid parenthesis in deeply nested function calls.
        * f $ g x is the same as f (g x).
        * $ 2 means "apply some function to 2", but since the function is missing, it's a partial application of $.
            So, ($ 2) is a function that takes another function and applies it to 2. => ($ 2) f = f 2

    Applicative Laws
In addition to providing the functions pure and <*>, applicative functors are also required to satisfy four equational laws:

    1. Identity:
    pure id <*> x      =     x       -- id : a -> a; x (lhs) : f a; x(rhs) : f a

    2. Homomorphism:
    pure (g x)              =     pure g <*> pure x       -- g: a -> b; x (lhs) :a; x (rhs): a

    3. Interchange:
    x <*> pure y       =    pure (\g -> g y) <*> x    -- x: f (a -> b); y : a; g: a -> b
                    or
    x <*> pure y       =    pure ($ y) <*> pure x

    4. Composition:
    x <*> (y <*> z)   =    (pure (.) <*> x <*> y) <*> z   -- x : f b -> c; y : f a -> b; z : f a

    The first equation states that pure preserves the identity function, in the sense that applying pure to this function gives
    an applicative version of the identity function.
    The second equation states that pure also preserves function application, in the sense that it distributes over normal
    function application to give applicative application.
    The third equation states that when an effectful function is applied to a pure argument, the order in which we evaluate
    the two components doesn't matter.
    The fourth equaiton states that, modulo the types that are involved, the operator <*> is associative.

    From: https://haskell.hpmeducation.com/functors-applicatives-and-monads/applicative-

    The `Identity` law states that applying the id function to an argument in applicative style returns the unaltered argument.

    The `Homomorphism` law states that pure preserves function application in the sense that applying a pure function to a
    pure value is the same as calling pure on the result of normal function application to that value  (f x).

    The `Interchange` states that the order in which we evaluate components does not matter in the case when we apply an
    effectful function to a pure argument.

    The `Composition` law states that function composition (.) works with the pure function as well, so that pure (.) composes
    functions, i.e., composing functions x and y with pure (.) and applying the composed function to z gives the same result
    as simply applying both functions x and y to the argument z.

    The fourth law reassociates applications to the left, the third law moves occurrences of pure to the left, and the remaining
    two laws allow zero or more consecutive occurrences of pure to be combined into one.

    Haskell also provides an infix version of fmap, defined by g <$> x = fmap g x, which in combination with the applicative
    law for fmap, gives an alternative formulation of applicative style:

        g <$> x1 <*> x2 <*> ... <*> xn

-}

{-
            Monads
    Monads provide another pattern of effectful programming.
-}
data Expr = Val Int | Div Expr Expr

eval :: Expr -> Int
eval (Val n) = n
eval (Div x y) = eval x `div` eval y -- doesn't account for the possibility of division by 0.

safediv :: Int -> Int -> Maybe Int
safediv _ 0 = Nothing
safediv x y = Just $ x `div` y

eval' :: Expr -> Maybe Int
eval' (Val n) = Just n
eval' (Div x y) =
  case eval' x of
    Nothing -> Nothing
    Just n -> case eval' y of
      Nothing -> Nothing
      Just m -> n `safediv` m

-- eval'' :: Expr -> Maybe Int
-- eval'' (Val n) = Just n
-- eval'' (Div x y) = pure safediv <*> eval'' x <*> eval'' y

{-
The above attempt is not equationally correct, since pure needs a function to be of type Int -> Int -> Int, whereas safediv
is of type Int -> Int -> Maybe Int. Replacing `pure safediv` with a custom function would also not help, because this function
would need to have type Maybe (Int -> Int -> Int), which does not provide any means to indicate failure when the second
argument is zero.

The conclusion is that the functgion eval does not fit the pattern of effectful programming that is captured by applicative
functors. The applicative style restricts us to applying pure functions to effectful arguments: eval does not fit this pattern
because the function safediv that is used to process the resulting values is not a pure function, but may itself fail.
-}

(>>=) :: Maybe a -> (a -> Maybe b) -> Maybe b
mx >>= f =
  case mx of
    Nothing -> Nothing
    Just x -> f x

{-
That is, >>= takes an argument of type a that may fail and a function of type a -> b whose result may fail, and returns a
result of type b that may fail. If the argument fails we propagate the failure, otherwise we apply the function to the resulting
value. In this manner, >>= integrates the sequencing of values of type Maybe with the processing of their results. The >>=
operator is often called bind, because the second argument binds the result of the first.
-}
eval'' :: Expr -> Maybe Int
eval'' (Val n) = Just n
eval'' (Div x y) =
  eval'' x Main.>>= \n ->
    eval'' y Main.>>= \m ->
      safediv n m

{-
    Generalizing from the above example, a typical expression that is built using the >>= operator has the following structure:

        m1 >>= \x1 ->
        m2 >>= \x2 ->
        .
        .
        .
        mn >>= \xn ->
        f x1 x2 ... xn

    That is, we evaluate each of the expressions m1 ... mn in turn, and then combine their result values x1 ... xn by applying the
    function f. The definition of the >>= operator ensures that such an expression only succeeds if every component m1 in the
    sequence succeeds. Moreover, the user doesn't have to worry about dealing with the possibility of failure at any point in the
    sequence, as this is handled automatically by the definition of the >>= operator.
    Haskell provides do notation to simplify the code:
        do x1 <- m1
             x2 <- m2
             .
             .
             .
             xn <- mn
             f x1 x2 ... xn
-}
eval''' :: Expr -> Maybe Int
eval''' (Val n) = Just n
eval''' (Div x y) = do
  n <- eval''' x
  m <- eval''' y
  n `safediv` m

{-
    The do notation is not specific to the types IO and Maybe, but can be used with any applicative type that forms a "monad".

        class Applicative m => Monad m where
            return :: a -> m a

            (>>=) :: m a -> (a -> m b) -> m b

            return = pure

    That is, a monad is an applicative type m that supports return and >>= functions of the specified types. The default definition
    return = pure means that return is normally just another name for the applicative function pure, but can be overridden in instance
    declarations if desired.

        Examples:

        instance Monad Maybe where
            -- (>>=) :: Maybe a -> (a -> Maybe b) -> Maybe b
                Nothing >>= _ = Nothing
                (Just a) >>= f = f a

        instance Monad [] where
            -- (>>=) :: [a] -> (a -> [b]) -> [b]
                as >>= f = [y | a <- as, y <- f a]

        That is, as >>= f applies the function f to each of the results in the list xs, collecting all the resulting values in a list. In this
        manner, the bind operator for lists provides a means of sequencing expressions that may produce multiple results.
-}
pairs :: [a] -> [b] -> [(a, b)]
pairs as bs = do
  a <- as
  b <- bs
  return (a, b)

-- The comprehension notation is specific to lists, whereas do notation can be used with an arbitrary monad.
pairs' :: [a] -> [b] -> [(a, b)]
pairs' as bs = [(a, b) | a <- as, b <- bs]

{-
    instance Monad IO where
    -- (>>=) :: IO a -> (a -> IO b) -> IO b
    ma >>= f = do {a <- ma; f a}

    This could have been the expected definition, but the definition is built into the language, rather than being defined in
    Haskell itself, so we cannot see them as we expect above.
-}

{-
        The state monad.

    This addresses the problem of writing functions that manipulate some form of state that can be changed over time.
-}
type State = Int -- this is for simplicity, It can be modified to fit whatever type is needed.

{-
    The most basic form of function on state type is a state transformer (ST), which takes an input state as its argument and
    produces an output state as its result, in which the output state reflects any updates that were made to the state by the
    function during its execution. In general, however, we may wish to return a result value in addition to updating the state.
    Types declared using type mechanism cannot be made into instances of classes, so we use the `newtype` mechanism.
-}
newtype ST a = S (State -> (a, State))

app :: ST a -> State -> (a, State)
app (S st) x = st x

instance Functor ST where
  -- fmap :: (a -> b) ->  ST a -> ST b
  fmap g st = S (\s -> let (x, s') = app st s in (g x, s'))

-- (<$>) = fmap: possibly this should have been done already.

{-
That is, fmap allows us to apply a function to the result value of a state transformer.
The let mechasism is similar to where mechanism, but it allows local definitions to be made at the level of expressions rather
than at the level of function definitions.

In this case, the function pure transforms a value into a state transformer that simply returns this value without modifying
the state.

In turn, the operator <*> applies a state transformer that returns a function to a state transformer that returns an argument
to give a state transformer that returns the result of applying the function to the argument.
-}
instance Applicative ST where
  -- pure :: a -> ST a
  pure x = S (\s -> (x, s))

  -- (<*>) :: ST (a -> b) -> ST a -> ST b
  stf <*> stx =
    S
      ( \s ->
          let (f, s') = app stf s
              (x, s'') = app stx s'
           in (f x, s'')
      )

{-
That is, st >>= f applies the state transformer st to an initial state s, then applies the function f to the resulting value x to
give a new state transformer f x, which is then applied to the new state s' to give the final result.

In this manner, the bind operator for the state monad integrates the sequencing of state transformers with the processing of
their result values. Note that within the definition for >>= we produce a new state transformer f x whose behavior may depend
on the result valure of the first argument x, whereas with <*> we are restricted to using state transformers that are explicitly
supplied as arguments. As such, using the >>= operator provides extra flexibility.
-}
instance Monad ST where
  -- (>>=) ST a -> (a -> ST b) -> ST b
  sta >>= f = S (\s -> let (x, s') = app sta s in app (f x) s')

data STree a = SLeaf a | SNode (STree a) (STree a) deriving (Show)

tree :: STree Char
tree = SNode (SNode (SLeaf 'a') (SLeaf 'b')) (SLeaf 'c')

rlabel :: STree a -> Int -> (STree Int, Int)
rlabel (SLeaf _) n = (SLeaf n, n + 1)
rlabel (SNode l r) n = (SNode l' r', n'')
  where
    (l', n') = rlabel l n
    (r', n'') = rlabel r n'

fresh :: ST Int
fresh = S (\n -> (n, n + 1))

alabel :: STree a -> ST (STree Int)
alabel (SLeaf _) = SLeaf <$> fresh
alabel (SNode l r) = SNode <$> alabel l <*> alabel r

{-
    > fst (rlabel tree 0)
    SNode (SNode (SLeaf 0) (SLeaf 1) ) (SLeaf 2)

    > fst (app (alabel tree) 0)
    SNode (SNode (SLeaf 0) (SLeaf 1) ) (SLeaf 2)

-}
mlabel :: STree a -> ST (STree Int)
mlabel (SLeaf _) = do
  n <- fresh
  return (SLeaf n)
mlabel (SNode l r) = do
  l' <- mlabel l
  r' <- mlabel r
  return (SNode l' r')

{-
    Generic Functions

An important benefit of abstracting out the concept of monads is the ability to define generic functions that can be used
with any monad. A number of such functions are provided in the library Control.Monad.
  -}
mapM :: (Monad m) => (a -> m b) -> [a] -> m [b]
mapM _ [] = return []
mapM f (x : xs) = do
  y <- f x
  ys <- Main.mapM f xs
  return (y : ys)

{-
Note that mapM has the same type as map, except that the argument function and the function itself now have monadic
return types.
-}
conv :: Char -> Maybe Int
conv c
  | isDigit c = Just (digitToInt c)
  | otherwise = Nothing

{-
    Applying mapM to the conv function gives a means of converting a string of digits into the corresponding list of numeric
    values, which succeeds if every character in the string is a digit, and fails otherwise:

    > mapM conv "1234"
    Just [1,2,3,4]

    > mapM conv "123a"
    Nothing
-}

filterM :: (Monad m) => (a -> m Bool) -> [a] -> m [a]
filterM _ [] = return []
filterM f (x : xs) = do
  y <- f x
  ys <- Main.filterM f xs
  return (if y then x : ys else ys)

{-
For example, in the case of list monad, using filterM provides a particularly concise means of computing the powerset of a
list, which is given by all possible ways of including or excluding each element of the list.

    > filterM (\_ -> [True,False]) [1,2,3]
    [[1,2,3], [1,2], [1,3], [1],[2,3], [2],[3],[]]
-}
join :: (Monad m) => m (m a) -> m a
join mmx = do
  mx <- mmx
  x <- mx
  return x

{-
        Monad Laws

   *  return x >>= f = f x;  x : a; f : a -> m b;

   * mx >>= return = mx;   mx : m a;

   * (mx >>= f) >>= g = mx >>= (\x -> (f x >>= g))  ; mx: m a; f : a -> m b; g: b -> m b;

   The first two equations concern the link between return and >>=. The first equation states that if we return a value and
   then feed this into a monadic function, this should give the same result as simply applying the function to the value.
   The second equation states that if we feed the result of a monadic computation into the function return, this should
   give the same result as simply performing the computation. Together these two equations state, modulo the fact that
   the second argument to >>= involves a binding operation, that return is the identity for >>= operator.

   The third equation concerns the link between >>= and itself, and expresses that >>= is associative.
-}

{-
        Exercises
-}

{-
Define an instance of the Functor class for the following type of binary trees that have data in their nodes.
-}
data ETree a = ELeaf | ENode (ETree a) a (ETree a) deriving (Show)

instance Functor ETree where
  -- fmap :: (a -> b) -> Tree a -> Tree b
  fmap _ ELeaf = ELeaf
  fmap f (ENode l a r) = ENode (fmap f l) (f a) (fmap f r)

{-
    Complete the following instance declaration to make the partially-applied function type (a -> ) into a functor

    Hint: first write down the type of fmap, and then think if you already know a library function that has this type:
-}
instance Functor ((->) a) where
  -- fmap :: (b -> c) -> (a -> b) -> (a -> c)
  fmap = (.)

{-
    Define an instance of the Applicative class for the type (a ->). If you are familiare with combinatory logic, you
    might recognize pure and <*> for this type as being the well-known K and S combinators.
-}
instance Applicative ((->) a) where
  -- pure :: b -> ( a-> b)
  pure = const

  -- <*> :: (a -> b -> c) -> (a->b) -> (a -> c)
  g <*> h = \x -> g x (h x)

{-
There may be more than one way to make a parameterized type into an applicative functor. For example the
library Control.Applicative provides an alternative `zippy` instance for lists, in which the function pure makes an
infinite list of copies of its argument, and the operator <*> applies each argument function to the corresponding
argument value at the same position.
Complete the following declarations that implement this idea:

The ZipList wrapper around the list type is required because each type can only have at most one instance
declaration for a given class.
-}
newtype ZipList a = Z [a] deriving (Show)

instance Functor ZipList where
  -- fmap :: (a -> b) -> ZipList a -> ZipList b
  fmap g (Z xs) = Z (map g xs)

instance Applicative ZipList where
  -- pure :: a -> ZipList a
  pure a = Z (repeat x)

  -- <*> :: ZipList ( a -> b) -> ZipList a -> ZipList b
  (Z fs) <*> (Z xs) = Z [g x | (g, x) <- zip gs xs]

{-
Work out the types for the variables in the four applicative laws.
-}

{-
Define an instance of the Monad class for the type (a ->).
-}

{-
Given the following type of expressions
    data Expr a = Var a | Val Int | Add (Expr a) (Expr a) deriving (Show)

that contain variables of some type a, show how to make this type into instances of Functor, Applicative and Monad
classes. With the aid of an example, explain what the >>= operator for this type does.
-}
data EExpr a = EVar a | EVal Int | EAdd (EExpr a) (EExpr a) deriving (Show)

instance Functor EExpr where
  -- fmap :: (a-> b) -> EExpr a -> EExpr b
  fmap f (EVar a) = EVar (f a)
  fmap _ (EVal i) = EVal i
  fmap f (EAdd (EExpr l) (EExpr r)) = EAdd (EExpr (fmap g l)) (EExpr (fmap g r))

instance Applicative EExpr where
  -- pure :: a -> Expr a
  pure (a :: Int) = EVal a
  pure a = EVar a

  -- <*> EExpr (a -> b) -> EExpr a -> EExpr b
  (EExpr f) <*> (EExpr a) = EExpr (fmap f a)

instance Monad EExpr where
  -- return :: a -> EExpr a
  return = pure

  -- (>>=) :: EExpr a -> (a -> EExpr b) -> EExpr b
  EExpr a >>= f = f a

{-
Rather than making a parameterized type into instances of the Functor, Applicative and Monad classes in this order,
in practice, it is sometimes simpler to define the functor and applicative instances in terms of the monad instance,
relying on the fact that the order in which declarations are made is not important in Haskell. Complete the missing
parts in the following declarations for the ST type using the do notation.
-}
newtype SST a = SS (State -> (a, State)) -- just another copy for ST type for the sake of this exercise.

-- The only confusion is, where to get the initial state from in the function declarations.
instance Functor SST where
  -- fmap :: (a -> b) -> SST a -> SST b
  fmap g sst = do
    x <- sst
    return g x

instance Applicative SST where
  -- pure :: a -> SST a
  pure x = SS (\s -> (x, s))

  -- <*> :: SST (a -> b) -> SST a -> SST b
  stf <*> stx = do
    f <- stf
    x <- stx
    return (f x)

instance Monad SST where
  -- (>>=) :: SST a -> (a -> SST b) -> SST b
  st >>= f =
    S
      ( \s ->
          let (x, s') = app st s in app (f x) s'
      )
