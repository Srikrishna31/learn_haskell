{-
    Lazey Evaluation

    The basic method of computation in Haskell is the application of functions to arguments.

    In Haskell, any two different ways of evaluating the same expression will always produce the same final value, provided
    that they both terminate.
    This property doesn't hold in general for most imperative programming languages, in which the basic method of
    computation is changing stored values. The precise time at which an assignment is performed in an imperative
    language may affect the value that results from a computation. In contrast, the time at which a function is applied to an
    argument in Haskell never affects the value that results from a computation.

        Evaluation Strategies

    An expression that has the form of a function applied to one or more arguments that can be `reduced` by performing
    the application is called a reducible expression or redex for short.

    When evaluating an expression, in what order should the reductions be performed?
    One common strategy, known as innermost evaluation, is to always choose a redex that is innermost, in the sense that
    it contains no other redex. If there is more than one innermost redex, by convention we choose the one that begins at
    the leftmost position in the expression. Innermost evaluation can also be characterized in terms of how arguments are
    passed to functions. In particular, using this strategy ensures that arguments are always fully evaluated before functions
    are applied. That is, arguments are passed by value.

    Another common strategy for evaluating an expression, dual to innermost evaluation, is to always choose a redex that
    is outermost, in the sense that it is contained in no other redex. If there is more than one such redex then as previously
    we choose that which begins at the leftmost position. This evaluation strategy is known as outermost evaluation. In terms
    of how arguments are passed to functions, using outermost evaluation allows functions to be applied before their arguments
    are evaluated. For this reason, we say that arguments are passed by name.

    Many built-in functions require their arguments to be evaluated before being applied, even when using outermost evaluation,
    for e.g. * and + cannot be applied until their two arguments have been evaluated to numbers. Functions with this property
    are called strict.

    When we define a curried function, which takes its arguments one at a time, the arguments are now substituted into the body
    of the function one at a time, rather than at the same time. In Haskell, the selection of redexes within the bodies lambda
    expressions is prohibited. The rationale for not 'reducing under lambdas' is that functions are viewed as black boxes that we
    are not permitted to look inside. The only operation that can be performed on a function is that of applying it to an argument.
    As such, reduction within the body of a function is only permitted once the function has been applied.

    Using innermost and outermost evaluation, but not within lambda expressions, is normally referred to as call-by-value and
    call-by-name evaluation, respectively.

-}
inf :: Int
inf = 1 + inf

{-
        Termination

    The above expression is non-terminating, since it is defined in terms of itself.

    Using call-by-value evaluation with the expression fst (0, inf) results in non-termination. In contrast, using call-by-name
    evaluation results in termination with the result zero in just one step, by immediately applying the definition of fst and
    hence avoiding the evaluation of the non-terminating expression inf.

    Call-by-name evaluation may produce a result when call-by-value evaluation fails to terminate. If there exists any evaluation
    sequence that terminates for a given expression, then call-by-name evaluation will also terminate for this expression, and
    produce the same final result. In summary, call-by-name evaluation is preferable to call-by-value for the purpose of
    ensuring that evaluation terminates as often as possible.

        Number of reductions

    In general, call-by-name evaluation may require more reduction steps than call-by-value evaluation, in particular when an
    argument is used more than once in the body of a function. Arguments are evaluated precisely once using call-by-value
    evaluation, but may be evaluated many times using call-by-name.

    Fortunately, the above efficiency problem with call-by-name evaluation can easily be solved, by using pointers to indicate
    sharing of expressions during evaluation. That is, rather than physically copying an argument if it is used many times in the
    body of a function, we simply keep one copy of the argument and make many pointers to it. In this manner, any reductions
    that are performed on the argument are automatically shared between each of the pointers to that argument.

    The use of call-by-name evaluation in conjunction with sharing is known as lazy evaluation. This is the evaluation strategy
    that is used in Haskell, as a result of which Haskell is known as a lazy programming language. Being based upon call-by-name
    convention, lazy evaluation has the property that it ensures that evaluation terminates as often as possible. Moreover,
    using sharing ensures that lazy evaluation never requires more steps than call-by-value evaluation.
    -}

ones :: [Int]
ones = 1 : ones

{-
    Infinite Structures

    An additional property of call-by-name evaluation, and hence lazy evaluation, is that it allows what at first may seem
    impossible: programming with infinite structures.
    Lazy evaluation proceeds in a lazy manner as its name suggests, only evaluating arguments as and when this is
    strictly necessary in order to produce results. For example, when selectring the first element of a list, the remainder of
    the list is not required, and hence in head (1:ones) the further evaluation of the infinite list ones is avoided. More
    generally, we have the following property: using lazy evaluation, expressions are only evaluated as much as required by
    the context in which they are used.

    Using this idea, we now see that under lazy evaluation, ones is not an infinite list as such, but rather a `potentially infinite`
    list, which is only evaluated as much as required by the context. This idea is not restricted to lists, but applies equally to
    any form of data structure in Haskell.

    Modular Programming

    Lazy evaluation also allows us to separate control from data in computations. That is, the data is only evaluated as much
    as required by the control, and these two parts take it in turn to perform reductions.
    Without lazy evaluation, the control and data parts would need to be combined in the form of a single function that
    produces a list of n identical elements such as replicate'
-}
take' :: Int -> [a] -> [a]
take' 0 _ = []
take' _ [] = []
take' n (x : xs) = x : take' (n - 1) xs

replicate' :: Int -> a -> [a]
replicate' 0 _ = []
replicate' n x = x : replicate' (n - 1) x

{-
    Sieve of Eratosthenes method for generating the infinite sequence of all prime numbers:

        * write down the infinite sequence 2,3,4,5,6,...
        * mark the first number, p, in the sequence as prime.
        * delete all multiples of p from the sequence;
        * return to second step.

    Note that the first and third steps each require an infinite amount of work, and hence in practice the steps must be
    interleaved.

    Looking at the implementation below, starting with the infinite list [2..] (step one), we apply the function sieve that retains the
    first number p as being prime (step two) and then calls itself recursively with a new list obtained by filtering all multiples of p
    from this list (steps three and four). Lazy evaluation ensures that this program does indeed produce the infinite list of all
    prime numbers.

    By freeing the generation of prime numbers from the constraint of finiteness, we have obtained a modular program on which
    different control parts can be used in different situations. For example, the first ten prime numbers and the prime numbers
    < 10, can be produced as follows:

        > take 10 primes
        [2,3,5,7,11,13,17,19,23,29]

        > takeWhile (<10) primes
        [2,3,5,7]
-}
primes :: [Int]
primes = sieve [2 ..]

sieve :: [Int] -> [Int]
sieve (p : xs) = p : sieve [x | x <- xs, x `mod` p /= 0]

{-
        Strict Evaluation

    Haskell uses lazy evaluation by default, but also provides a special strict version of function application, written as `$!`, which
    can sometimes be useful. Informally, an expression of the form f $! x behaves in the same way as the normal functional
    application of  f x, except that the top-level of evaluation of the argument expression x is forced before the function f is applied.

    For example, if the argument has a basic type, such as Int or Bool, then top-level evaluation is simply complete evaluation. On
    the other hand, for a pair type such as (Int, Bool), evaluation is performed until a pair of expressions is obtained, but no further.
    SImilarly for a list type, evaluation is performed until the empty list or the cons of two expressions is obtained.

    More formally, an expression of the form f $! x is only a redex once evaluation of the argument x, using lazy evaluation as normal,
    has reached the point where it is known that the result is not an undefined value, at which point the expression can be reduced
    to the normal application f x.

    When used with a curried function with multiple arguments, strict application can be used to force top-level evaluation of any
    combination of arguments. For example, if f is a curried function with two arguments, an application of the form f x y can be
    modified to have three different behaviors:

        (f $! x) y  -- forces top-level evaluation of x

        (f x) $y  -- forces top-level evaluation of y

        (f $! x) $! y  -- forces top-level evaluation of x and y.

    In Haskell, strict application is mainly used to improve the space performance of programs.
-}
sumWith :: Int -> [Int] -> Int
sumWith v [] = v
sumWith v (x : xs) = sumWith (v + x) xs

{-
    Note that the entire summation ((0 + 1) + 2) + 3) ... is constructed before any of the component additions are actually performed.
    More generally, sumWith will construct a summation whose size is proportional to the number of integers in the original list, which
    for a long list may require a significant amount of space. In practice, it would be preferable to perform each addition as soon as it
    is introduced, to improve the space performance of the function. This behavior can be achieved as follows:
-}
sumWith' :: Int -> [Int] -> Int
sumWith' v [] = v
sumWith' v (x : xs) = (sumWith' $! (v + x)) xs

{-
    Generalizing the above example, the library Data.Foldable provides a strict version of the higher-order library function foldl that
    forces evaluation of its accumulator prior to processing the tail of the list:
-}
foldl' :: (a -> b -> a) -> a -> [b] -> a
foldl' _ v [] = v
foldl' f v (x : xs) = (Main.foldl' f $! (f v x)) xs

{-
    Even for relatively simple examples, the use of strict application is a specialist topic that requires careful consideration of the
    behavior of lazy evaluation.
-}

{-
        Exercises
-}

{-
    Identify the redexes in the following expressions, and determine whether each redex is innermost, outermost, neither, or both:

        1 + (2*3)             -- neither as there is no function application.

        (1+2) * (2+3)        -- neither as there is no function application

        fst (1+2, 2+3)      -- both since it is an independent redex.

        (\x -> 1 + x) (2*3)   -- both since it is an independent redex.
-}

{-
    Show why outermost evaluation is preferable to innermost for the purposes of evaluating the expression fst (1+2, 2+3)

    For innermost evaluation, it will evaluate both expressions: 1+2, 2+3 before applying the function, whereas in outermost
    evaluation, the function is first applied so that it becomes 1+2, so the unnecessary computation of 2+3 is not performed,
    thereby gaining some efficiency.
-}

{-
    Given the definition mult = \x -> (\y -> x * y), show how the evaluation of mult 3 4 can be broken down into four separate
    steps:

        mult 3 4
            {applying mult}
        \x -> (\y -> x * y) 3 4
            {applying first lambda}
        (\y -> 3 * y) 4
            {applying second lambda}
        3 * 4
            {applying *}
        12
-}

{-
    Using a list comprehension, define an expression fibs::[Integer] that generates the infinite sequence of Fibonacci numbers
        0, 1, 1, 2, 3, 5, 8, 13, 21, 34, ...

    using the following simple procedure:
        * the first two numbers are 0 and 1
        * the next is the sum of the previous two
        * return to the second step.

    Hint: make use of the library functions zip and tail. Note that numbers in the Fibonacci sequence quickly become large, hence
    the use of the type Integer of arbitrary-precision integers above.
-}
fibs :: [Integer]
fibs = 0 : 1 : [a + b | (a, b) <- zip fibs (tail fibs)] -- taken from chatGPT.

{-
    Define appropriate versions of the library functions

        repeat :: a -> [a]
        repeat x = xs where xs = x:xs

        take :: In t-> [a] -> [a]
        take 0 _ = []
        take _ [] = []
        take n (x:xs) = x : take (n-1) xs

        replicate :: Int -> a -> [a]
        replicate n = take n . repeat
-}
data Tree a = Leaf | Node (Tree a) a (Tree a) deriving (Show)

repeat' :: a -> Tree a
repeat' x = Node (repeat' x) x (repeat' x)

take'' :: Int -> Tree a -> [a]
take'' n nd = vs
  where
    (vs, _) = go n nd

    go :: Int -> Tree a -> ([a], Int)
    go 0 _ = ([], 0)
    go n Leaf = ([], n)
    go n (Node l a r) = (a : (ltree ++ rtree), final)
      where
        (ltree, rem) = (go $! (n - 1)) l
        (rtree, final) = if rem == n then (go $! (rem - 1)) r else go rem r -- somehow the right part of the tree is not being considered properly in this implementation.

replicate'' :: Int -> a -> [a]
replicate'' n = take'' n . repeat'

{-
    Newton's method for computeing square root of a (non-negative) floating-point number n can be expressed as follows:
        * start with an initial approximation to the result.
        * given the current approximation a, the next approximation is defined by the function next a = (a + n/a) / 2;
        * repeat the second step until the two most recent approximations are within some desired distance of one another, at
        which point the most recent value is returned as the result.

    Define a function sqroot :: Double -> Double that implements this procedure. Hint: first produce an infinite list of approximations
    using the library function iterate. For simplicity take the number 1.0 as the initial approximation, and 0.00001 as the distance value.
-}

sqroot :: Double -> Double
sqroot n = r2
  where
    (r1, r2) = head (dropWhile (\(x1, x2) -> abs (x1 - x2) > 0.0001) approxPairs)
    approxPairs = zip nApprox (tail nApprox)
    nApprox = approx n
    approx :: Double -> [Double]
    approx n = iterate (\a -> (a + n / a) / 2) 1.0
