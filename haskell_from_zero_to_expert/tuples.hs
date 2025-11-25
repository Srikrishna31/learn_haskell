{-
A tuple is a structured type that allows us to store different type values t1, t2, .., tn on a single value of type (t1,t2,...tn)
\* The number of fields is fIxed
\* The fields are of heterogeneous type.

-}

mostFrequentCharacter :: String -> (Char, Int)
mostFrequentCharacter "AVATAR" = ('A', 3)

timeDecomposition :: Int -> (Int, Int, Int)
timeDecomposition seconds = (h, m, s)
  where
    h = seconds `div` 3600
    m = (seconds `mod` 3600) `div` 60
    s = seconds `mod` 60

-- fst :: (a, b) -> a

-- snd :: (a, b) -> b
--
-- Decomposition of Tuples into patterns
--
distance :: (Float, Float) -> (Float, Float) -> Float
distance p1 p2 = sqrt ((fst p1 - fst p2) ^ 2 + (snd p1 - snd p2) ^ 2)

distance' :: (Float, Float) -> (Float, Float) -> Float
distance' (x1, y1) (x2, y2) = sqrt ((x1 - x2) ^ 2 + (y1 - y2) ^ 2)

distance'' :: (Float, Float) -> (Float, Float) -> Float
distance'' p1 p2 = sqrt (sqr dx + sqr dy)
  where
    (x1, y1) = p1
    (x2, y2) = p2
    dx = x1 - x2
    dy = y1 - y2
    sqr x = x * x

{-
Empty Tuple (Unit)
There exists the tuple type without any data, which has only one possible value: the empty data.

Conecpt similar to void in C.

Type: ()
Value: ()
-}
