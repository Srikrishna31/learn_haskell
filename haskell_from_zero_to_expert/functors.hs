{-
    Functors
    Fmap
fmap applies a function to the elements of a generic container f a returning a container of same type.
fmap is a function of the instances of the class Functor.

<$> is the infix version of fmap.

functions are also instances of Functor.

instance Functor ((->) r) where        -- It should be read as a function which accepts any input type, and returns a type r
    fmap = (.)
instance Functor ((,) a) where
    fmap f (x, y) = (x, f y)

fmap :: Functor f => (a -> b) -> f a -> f b
-}

data Bintree a
  = Empty
  | Node a (Bintree a) (Bintree a)
  deriving (Show)

instance Functor Bintree where
  fmap f Empty = Empty
  fmap f (Node a l r) = Node (f a) (fmap f l) (fmap f r)

tree1 :: Bintree Int
tree1 = Node 3 Empty (Node 2 (Node 1 Empty Empty) (Node 1 Empty Empty))

tree2 :: Bintree Int
tree2 = Node 6 Empty (Node 4 (Node 2 Empty Empty) (Node 2 Empty Empty))

{-
    Laws of Functors

    1. Identity: fmap id = id
    2. Composition: fmap (g1. g2) = fmap g1 . fmap g2

-}
