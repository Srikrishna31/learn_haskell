import Control.Monad.State

{-
        Monads

Consider the `half` function which only makes sense on even numbers.

We can see the function like this: Given a value, return a packed value.
But then we can't stuff packed values into it.
-}
half :: Int -> Maybe Int
half x
  | even x = Just (div x 2)
  | otherwise = Nothing

{-
    We need a function that unpacks, applies `half` and leaves encapsulated.

    This function is called >>= (bind)

    Basically, a monad applies a packaged value to a function accepting only a value, and keeps the packaging intact.

    The operator >>= is an operation of the class `Monad`:

    class Applicative m => Monad m where
        return :: a -> m a      -- wrap
        (>>=) :: m a -> (a -> m b) -> m b    -- unwrap, apply and wrap.
        (>>) :: m a -> m b -> m b  -- is purely esthetic.
            r >> k = r >>= (\_ -> k)

    The type `Maybe` is an instance of `Monad`:

    instance Monad Maybe where
        return = Just
        Nothing >>= f = Nothing
        Just x >>= f = f x

    instance Monad (Either a) where
        return = Right
        Left x >>= f = Left x
        Right a >>= f = f a

    instance Monad [] where
        return x = [x]
        xs >>= f = concatMap f xs

        Monad Laws

    1. Identity on the left

        return x >>= f = f x

    2. Identity on the right

        m >>= return = m

    3. Associativity

    (m >>= f) >>= g = m >>= (\x -> f  x >>= g)

        Do notation

    The do notation is syntactic sugar to facilitate the use of monads. With do, functional code looks like imperative code
    with assignments.

    The computations can be sequenced:

        do {c1;c2}
        ==
        do
            c1
            c2
        ==
        c1 >> c2
        ==
        c1 >>= \_ -> c2

    And with <-, extract its results

    do {x <- c1; c2}
    ==
    do x <- c1
        c2

    ==
    c1 >>= \x -> c2
-}

data Model = Clio | Audi | Cadillac | Nissan deriving (Eq, Show)

data Label = B | C | Eco | Zero deriving (Eq, Show)

registrations = [("Albert", 3526), ("Peter", 8427), ("Sofia", 7383), ("Olivia", 5913)]

models = [(3526, Audi), (8427, Clio), (7383, Nissan), (5913, Cadillac)]

labels = [(Clio, Zero), (Audi, C), (Cadillac, B), (Nissan, Eco)]

-- Given an owner name, we want to know what their emissions label is:
label :: String -> Maybe Label
label owner =
  do
    registration <- Main.lookup owner registrations
    model <- Main.lookup registration models
    Main.lookup model labels

labelFunctional :: String -> Maybe Label
labelFunctional owner =
  Main.lookup owner registrations >>= \reg ->
    Main.lookup reg models >>= \mod ->
      Main.lookup mod labels

-- It's Maybe because, maybe the owner doesn't exist, or we don't have his registration,
-- or we don't have his model, or we don't have his label.
-- A useful predefined function to use:
lookup :: (Eq a) => a -> [(a, b)] -> Maybe b
lookup v ls = if null f then Nothing else Just ((snd . head) f)
  where
    f = filter (\(x, _) -> x == v) ls

{-
    State Monad

    The `State Monad` in Haskell is a powerful tool for managing and manipulating state within a functional programming
    paradigm.
    It allows us to write code that appears to be purely functional while still encapsulating state changes in a clean and
    predictable manner.
    Understanding the state monad is essential for functional programming in Haskell and can greatly enhance our ability
    to write elegant and maintainable code.

    State

    State represents the current condition or context of a program and in purely functional programming, managing state
    can be challenging.

    Immutability and Referential Transparency

    Immutability is a core principle of functional programming. It means that once data is created, it cannot be changed. This
    immutability ensures that data remains constant throughout it's lifetime, making it easier to reason about code and
    preventing unintended side effects.

    Referential Transparency is another key concept in functional programming. It means a function's output depends solely
    on its input and calling a function with the same inputs will always produce the same result. This property allows for
    predictable and reliable code because functions don't have hidden side effects

    State Monad

    State type: Represents a stateful computation.

    State constructor: Wraps a function that takes the initial state and produces a result and a new state.

    runState function: Executes the stateful computation.

    newType State s a = State { runState :: s -> (a, s)}

    With the State monad, we can manage state in a purely functional manner while maintaining the principles of immutability
    and referential transparency.
-}

incrementCounter :: State Int ()
incrementCounter =
  do
    counter <- get
    put (counter + 1)

decrementCounter :: State Int ()
decrementCounter =
  do
    counter <- get
    put (counter - 1)

main :: IO ()
main =
  let initialState = 0

      finalState = execState (do incrementCounter; incrementCounter; decrementCounter) initialState
   in putStrLn $ "Final State " ++ show finalState
