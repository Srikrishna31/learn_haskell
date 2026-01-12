{-
        Monoids

    In mathematics, a monoid is a set together with an associative operator that combines two elements from the set, and an
    identity element for the operator. For example, the set of integers forms a monoid with the operator given by addition and
    the identity element by the value zero.

        class Monoid a where
            mempty :: a

            mappend :: a -> a -> a

            mconcat :: [a] -> a
            mconcat = foldr mappend mempty

    That is, for a type a to be an instance of the class Monoid, it must support a value mempty and a function mappend of
    the specified types, which respectively play the role of the identity element and the operator for the monoid.
    As well as the two primitives, the Monoid class also provides a function mconcat that combines a list of values within a
    monoid, with a default definition that replaces each cons in the list by mappend and the empty list by mempty.

    The two primitives in the Monoid class are required to satisfy the following identity and associativity laws:

        mempty 'mappend'  x = x

        x `mappend` mempty  = x

        x `mappend' (y `mappend` z) = (x `mappend` y) `mappend` z

            Examples

        instance Monoid [a] where
            -- mempty :: [a]
            mempty = []

            -- mappend :: [a] -> [a] -> [a]
            mappend = (++)

        instance Monoid a => Monoid (Maybe a) where
            -- mempty :: Maybe a
            mempty = Nothing

            -- mappend :: Maybe a -> Maybe a -> Maybe a
            mappend (Just a) (Just b) = Just (a <> b)
            mappend mx Nothing = mx
            mappend Nothing my = my

    Integers form a monoid under addition, and multiplication operations with 0 and 1 as identity/empty elements respectively.

-}
newtype Sum a = Sum a deriving (Eq, Ord, Show, Read)

getSum :: Sum a -> a
getSum (Sum x) = x

-- instance (Num a) => Monoid (Sum a) where
--   -- mempty :: Sum a
--   mempty = Sum 0

--   -- mappend :: Sum a -> Sum a -> Sum a
--   mappend (Sum a) (Sum b) = Sum (a + b)

newtype Product a = Product a deriving (Eq, Ord, Show, Read)

getProduct :: Product a -> a
getProduct (Product a) = a

-- instance (Num a) => Monoid (Product a) where
--   -- mempty :: Product a
--   mempty = Product 1

--   -- mappend :: Product a -> Product a -> Product a
--   mappend (Product a) (Product b) = Product (a * b)

{-
In a similar manner, the type of logical values forms a monoid under both logical conjunction and disjunction, for which
purpose the monid library provides wrapper types for Bool called All and Any.
The library also provides an infix version of mappend, defined by x <> y = x `mappend` y, which allows monoid expressions
to be written concisely.
-}

{-
        Foldables

    One of the primary application of monoids is to combine all the values in a data structure to give a single value.
-}
fold :: (Monoid a) => [a] -> a
fold [] = mempty
fold (x : xs) = x `mappend` fold xs

{-
    That is, applying fold to an empty list gives the identity element mempty of the monoid, while for a non-empty list we
    use the monoid operator mappend to combine the head of the list with the result of recursively processing the tail.
    In other words, fold provides a simple means of folding up a list using a monoid, hence the choice of name for the
    function.
-}
data Tree a = Leaf a | Node (Tree a) (Tree a) deriving (Show)

foldt :: (Monoid a) => Tree a -> a
foldt (Leaf x) = x
foldt (Node l r) = foldt l `mappend` foldt r

{-
    More generally, the idea of folding up the values in data structure using a monoid isn't specific to types such as lists
    and binary trees, but can be abstracted to a range of parameterized types. In Haskell this concept is captured by the
    following class declaration in the library Data.Foldable:

        class Foldable t where
            fold :: Monoid a => t a -> a

            foldMap :: Monoid b => (a -> b) -> t a -> b

            foldr :: (a -> b -> b) -> b -> t a -> b

            foldl :: (a -> b -> a) -> a -> t b -> a

    That is, for a parameterized type to be an instance of the class Foldable, it must support a range of fold functions of
    the specified types.
    Intuitively, the generalized version of fold in the Foldable class takes a data structure of type t a whose elements have
    type a, and combines the elements using the monoid primitives for this type to give a single value of type a. In turn,
    foldMap generalizes fold by taking a function of type a -> b as an additional argument, which is applied to each
    element in the structure prior to combining the resulting values using the monoid primitives for the type b.

        instance Foldable [] where
            -- fold :: Monoid a => [a] -> a
            fold []         = mempty
            fold (x:xs)   = x `mappend` fold xs

            -- foldMap :: Monoid b => (a -> b) -> [a] -> b
            foldMap _ [] = mempty
            foldMap f (x:xs) = f x `mappend` foldMap f xs

            -- foldr :: (a -> b  -> b) -> b -> [a] -> b
            foldr _ v [] = v
            foldr f v (x:xs) = f x (foldr f v xs)

            -- foldl :: (a -> b -> a) -> a -> [b] -> a
            foldl _ v [] = v
            foldl f v (x:xs) = foldl f (f v x) xs
-}
instance Foldable Tree where
  -- fold :: Monoid a => Tree a -> a
  -- fold (Leaf x) = x
  -- fold (Node l r) = fold l `mappend` fold r

  -- foldMap :: Monoid b => ( a -> b)  -> Tree a -> b
  foldMap f (Leaf x) = f x
  foldMap f (Node l r) = (foldMap f l) `mappend` (foldMap f r)

  -- foldr :: (a -> b -> b) -> b -> [a] -> b
  foldr f v (Leaf x) = f x v
  foldr f v (Node l r) = foldr f (foldr f v r) l

  -- foldl :: (a -> b -> a) -> a -> [b] -> a
  foldl f v (Leaf x) = f v x
  foldl f v (Node l r) = foldl f (foldl f v l) r

tree :: Tree Int
tree = Node (Node (Leaf 1) (Leaf 2)) (Leaf 3)

{-
    In addition to the four basic folding primitives, the Foldable class also includes a range of other useful functions
    for combining the values in a data structure. The first group generalise familiar functions on lists:

        null :: t a -> Bool
        length :: t a -> Int
        elem :: Eq a => a -> t a -> Bool
        maximum :: Ord a => t a -> a
        minimum :: Ord a => t a -> a
        sum :: Num a => t a -> a
        product :: Num a => t a -> a

    In turn, the class also includes versions of foldr and foldl for structures that contain at least one element, and
    hence do not require a starting value:

        foldr1 :: ( a -> a -> a) -> t a -> a
        foldl1 :: (a -> a -> a) -> t a -> a

    The final primitive flattens a data structure to a list, such as transforming the tree Node (Leaf 1) (Leaf 2) into
    the list [1,2]

        toList :: t a -> [a]

    In fact, the function toList plays a special role in the declaration of the Foldable class, as it can be used to
    provide default definitions for most of the other primitives in the class in terms of the corresponding
    primitives for lists.

        foldr f v = foldr f v . toList
        foldl f v = fold f v . toList
        foldr1 f = foldr1 f . toList
        foldl1 f = foldl1 f . toList

        null = null . toList
        length = length . toList
        elem x = elem x . toList
        maximum = maximum.toList
        minimum = minimum . toList
        sum = sum . toList
        product = product . toList

    The final three default definitions in the foldable class establish important relationships between the
    primitives fold, foldMap and toList:

        fold = foldMap id
        foldMap f = foldr (mappend . f) mempty
        toList = foldMap (\x -> [x])

    That is, fold can be viewed as a special case of foldMap where the identity function is applied to each
    element prior to combining them. In turn, foldMap can be defined in terms of foldr by applying the
    function f to each element before they are combined using the monoid primitives. And finally, toList
    can be defined in terms of foldMap by first transforming each element into a singleton list, and then
    concatenating the resulting lists using the list monoid.

    1. Why are there so many functions in the Foldable class? To allow for implementations to override
    the default implementations, which is not possible if they were top-level library functions.
    2. What do we need to define manually? The minimal complete definition for an instance of the
    Foldable class is to define either foldMap or foldr. It is often simplest to define the foldMap function.
    3. What about efficiency? For many applications, using the default definitions that are provided in
    the class will suffice, but if greater efficiency is required these can be overridden.

    and :: (Foldable t) => t Bool -> Bool
    and = getAll . foldMap All

    or :: (Foldable t) => t Bool -> Bool
    or = getAny . foldMap Any

    all :: (Foldable t) => (a -> Bool) -> t a -> Bool
    all p = getAll . foldMap (All . p)

    any :: (Foldable t) => (a -> Bool) -> t a -> Bool
    any p = getAny . foldMap (Any . p)

-}
average :: (Foldable t) => t Int -> Int
average ns = sum ns `div` length ns

-- concat :: (Foldable t) => t [a] -> [a]
-- concat = fold

{-
    In conclusion, when declaring a new type in Haskell it is useful to consider whether it can be made
    into a foldable type, for which it suffices to define either of the primitives foldMap or foldr. The
    advantage of doing so is that we are then provided with a range of useful functions for the type
    essentially for free by means of the default definitions that are included in the Foldable class, as
    well as any other generic functions defined in terms of these primitives.
-}

{-
        Traversables

    The class of types that support a genaralized mapping function are called traversable types, or
    traversables for short.

        class (Functor t, Foldable t) => Traversable t where
            traverse :: Applicative f => (a -> f b) -> t a -> f (t b)

    That is, for a parameterized type t that is both functorial and foldable to be an instance of the
    class Traversable, it must support a traverse function and the specified type. The requirement
    that t is a functor reflects the fact that traversable generalizes the idea of mapping, and are hence
    expected to support the fmap primitive. The requirement that t is a foldable ensures that values
    in a traversable type can also be folded up if desired.

        Example:

        instance Traversable [] where
            --traverse :: Applicative f => (a -> f b) -> [a] -> f [b]
            traverse _ [] = pure []
            traverse g (x:xs) = pure (:) <*> g x <*> traverse g xs
-}
traverse' :: (a -> Maybe b) -> [a] -> Maybe [b]
traverse' _ [] = pure []
traverse' f (x : xs) = pure (:) <*> f x <*> traverse f xs

instance Functor Tree where
  -- fmap :: (a -> b) -> Tree a -> Tree b
  fmap g (Leaf x) = Leaf (g x)
  fmap g (Node l r) = Node (fmap g l) (fmap g r)

instance Traversable Tree where
  -- traverse :: Applicative f => ( a -> f b) -> Tree a -> f (Tree b)
  traverse p (Leaf a) = pure Leaf <*> p a
  traverse p (Node l r) = pure Node <*> traverse p l <*> traverse p r

{-
    Other primitives

        class (Functor t, Foldable t) => Traversable t where
            traverse :: Application f => ( a -> f b) -> t a -> f (t b)

            sequenceA :: Applicative f => t (f a) -> f (t a)
            sequenceA = traverse id

            --traverse :: Applicative f => (a -> f b) -> t a -> f (t b)
            traverse g = sequnceA . fmap g
     The type expresses that sequenceA transfomrs a datastructure whose elements are applicative
     actions into a single such action that returns a data structure, while the definition states that this
     can be achieved by traversing the elements of the structure using the identity function, which in
     this case has the type f a -> f a.
     Conversely, the class declaration also includes a default definition for traverse in terms of
     sequenceA, which expresses that to traverse a data structure using an effectful function we can
     first apply the function to each element using fmap and then combine all the effects using sequenceA.

     In this manner, to declare an instance of the Traversable class, it suffices to define either traverse
     or sequenceA, as each can be derived from the other using the above defaults.

     Finally the class also provides special names for the two traversable primitives for the special case
     when the effects that are involved are monadic rather than applicative as shown below.

        mapM :: Monad m => (a -> m b) -> t a -> m (t b)
        mapM = traverse

        sequence :: Monad m => t (m a) -> m (t a)
        sequence = sequenceA

    In conclusion, when declaring a new type it is also useful to consider whether it can be made into
    a traversable type, by defining either of the primitives traverse or sequenceA. The advantage of
    doing so is that we are then provided with a number of useful functions for effectful programming
    with the type, by means of the default definitions in the Traversable class.
-}

{-
        Exercises
-}

{-
    Complete the following instance declaration from Data.Monoid to make a pair type into a monoid
    provided the two component types are monoids:
-}
instance (Monoid a, Monoid b) => Monoid (a, b) where
  -- mempty :: (a,b)
  mempty = (mempty, mempty)

  -- mappend :: (a,b) -> (a,b) -> (a,b)
  mappend (a1, b1) (a2, b2) = (a1 `mappend` a2, b1 `mappend` b2)

{-
    In a similar manner, show how a function type a -> b can be made into a monoid provided that
    the result type b is a monid.
-}
instance (Monoid b) => Monoid (a -> b) where
  -- mempty :: a -> b
  mempty _ = mempty :: b

  -- mappend :: (a -> b) -> (a -> b) -> (a -> b)
  mappend f1 f2 x = f1 x `mappend` f2 x

{-
    Show how the Maybe type can be made foldable and traversable, by giving explicit definitions for
    fold, foldMap, foldr, foldl and traverse.
-}
instance Foldable Maybe where
  -- fold :: Monoid a => Maybe a -> a
  fold Nothing = mempty :: a
  fold (Just x) = x

  -- foldMap :: Monoid b => (a -> b) -> Maybe a -> b
  foldMap _ Nothing = mempty :: b
  foldMap f (Just x) = f x

  -- foldr :: (a -> b -> b) -> b -> Maybe a -> b
  foldr _ v Nothing = v
  foldr f v (Just x) = f x v

  -- foldl :: (a -> b -> b) -> a -> Maybe b -> a
  foldl _ v Nothing = v
  foldl f v (Just x) = f v x

instance Traversable Maybe where
  -- traverse :: Applicative f => (a -> f b) -> Maybe a -> f (Maybe b)
  traverse _ Nothing = pure Nothing
  traverse f (Just x) = pure Just <*> f x

{-
    In a similar manner, show how the following type of binary trees with data in their nodes can be
    made into a foldable and traversable type:
-}
data ETree a = ELeaf | ENode (ETree a) a (ETree a) deriving (Show)

instance Foldable ETree where
  -- fold :: Monoid a => ETree a -> a
  fold _ ELeaf = mempty :: a
  fold f (ENode l a r) = fold f l `mappend` f a `mappend` fold f r

  -- foldMap :: Monoid b => (a -> b) -> ETree a -> b
  foldMap _ ELeaf = mempty :: b
  foldMap f (ENode l a r) = foldMap f l `mappend` f a `mappend` foldMap f r

-- foldr :: (a -> b -> b) -> b -> ETree a -> b
foldr _ v ELeaf = v
foldr f v (ENode l a r) = Main.foldr f (f (Main.foldr f v l) a) r

-- foldl :: (a -> b -> b) -> a -> ETree b -> a
foldl _ v ELeaf = v
foldl f v (ENode l a r) = Main.foldl f (f (Main.foldl f v r) a) l

instance Traversable ETree

-- traverse :: Applicative f => (a -> f b) -> ETree a -> f (ETree b)
traverse _ ELeaf = pure ELeaf
traverse f (ENode l a r) = Main.traverse f l <*> pure a <*> Main.traverse f r

{-
    Using foldMap, define a generic version of the higher-order function filter on lists that can be
    used with any foldable type:
-}
filterF :: (Foldable t) => (a -> Bool) -> t a -> [a]
filterF p ta = foldMap (\x -> if p x then [p] else []) ta
