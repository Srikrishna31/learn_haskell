{-
    Applicatives

Applicatives are nothing but applying functions in a container to values in another container, where the container is the same type.
It is represented by this operator: <*>

The operator <*> is an operation of the class Applicative (which must also be a functor)

class Functor f => Applicative f where
    (<*>) :: f (a -> b) -> (f a -> f b)
    pure :: a -> f a

<*> applies a function inside a container to values inside a container. Containers are generic and of the same type.
pure constructs a container with a value.

The operator <*> is not commutative - meaning, the left operand should always be a container of functions, and the right operand should always be
the container of values for it to work.

Laws of Applicatives

1. Identity: pure id <*> v = v

2. Homomorphism: pure f <*> pure x = pure (f x)

3. Exchange: u <*> pure y = pure ($ y) <*> u

4. Composition: u <*> (v <*> w) = pure (.) <*> u <*> v <*> w

5. Relation with the functor: fmap g x = pure g <*> x

instance Applicative Maybe where
    pure = Just
    Nothing <*> _ = Nothing
    Just f <*> x = fmap f x

instance Applicative (Either a) where
    pure = Right
    Left x <*> _ = Left x
    Right f <*> x = fmap f x

instance Applicative (List a) where
    pure a=  [a]
    [] <*> _ = []
    (f:fs) <*> ls = (concatMap f ls) ++ (fs <*> ls)

instance Applicative [] where
    pure x = [x]
    fs <*> xs = [f x | f <- fs, x <- xs]
-}
