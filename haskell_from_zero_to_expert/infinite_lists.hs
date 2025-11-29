-- The goal of this problem is to work the definition of infinite lists. In particular, you are required to define the function that generates the
-- sequence of ones [1,1,1,1,1...]. Use the function ones ::[Integer]
ones :: [Integer]
ones = repeat 1

ones' :: [Integer]
ones' = [1, 1 ..]

-- The goal of this problem is to work the definition of infinite lists. In  particular, you are required to define the function that generates the
-- sequence of natural numbers [0, 1, 2, 3, 4, 5, ...]. Use the function nats :: [Integer]
nats :: [Integer]
nats = iterate (+ 1) 0

-- The goal of this problem is to work the definition of infinite lists. In particular, you are required to define the function that generates the sequence of
-- the integer numbers [0, 1, -1, 2, -2, 3, -3, ...]. Use the function ints::[Integer]
ints :: [Integer]
ints = 0 : [y | x <- iterate (+ 1) 1, y <- [x, -x]]

ints' :: [Integer]
ints' = iterate integers 0
  where
    integers :: Integer -> Integer
    integers x
      | x > 0 = -x
      | otherwise = 1 - x

-- The goal of this problem is to work the definition of infinite lists. In particular you are required to define the function that generates the
-- sequence of the triangular numbers  [0, 1, 3, 6, 10, 15, 21,28,...]. Use the function triangulars :: [Integer]
-- The Triangular numbers is a representation of the numbers in an equilateral triangle triangle, arranged in a series.
triangular :: [Integer]
triangular = [x * (x + 1) `div` 2 | x <- nats]

triangular' :: [Integer]
triangular' = scanl (+) 0 $ iterate (+ 1) 1

myRepeat :: a -> [a]
myRepeat x = x : myRepeat x

-- The goal of this problem is to work the definition of infinite lists. In particular you are required to define the function that generates the
-- sequence of the factorial numbers [1, 1, 2, 6,  24, 120, 720, 5040,...]. Use the function factorial :: [Integer]
factorials :: [Integer]
factorials = scanl (*) 1 $ iterate (+ 1) 1

fibs = scanl (+) 1 $ iterate (+ 1) 1

-- The goal of this problem is to work the definition of infinite lists. In particular you are required to define the function that generates the sequence of the
-- Fibonacci numbers [0,1,1,2,3,5,8,13...]
fibonacci :: [Integer]
fibonacci = go 0 1
  where
    go :: Integer -> Integer -> [Integer]
    go f1 f2 = f1 : go f2 (f1 + f2)

-- The goal of this problem is to work the definition of infinite lists. In particular you are required to define the function that generates the sequence of the
-- prime numbers [2,3,5,7,11,13,17,19,..]. Use the function primes::[Integer]
primes :: [Integer]
primes = 2 : filter isPrime (iterate (+ 2) 3)
  where
    isPrime :: Integer -> Bool
    isPrime n = all (\x -> n `mod` x /= 0) [3, 5 .. n_half]
      where
        -- n_sqrt = floor $ sqrt $ fromIntegral n
        n_half = n `div` 2

primes' :: [Integer]
primes' = 2 : filter isPrime (iterate (+ 2) 3)
  where
    isPrime :: Integer -> Bool
    isPrime n = isPrimeAux 3
      where
        isPrimeAux :: Integer -> Bool
        isPrimeAux x
          | x >= n `div` 2 = True
          | n `mod` x == 0 = False
          | otherwise = isPrimeAux (x + 2)

-- The goal of this problem is to work the definition of infinite lists. In particular you are required to define the function that
-- generates the ordered sequence of the Hamming numbers [1, 2, 3, 4, 5, 6, 8, 9, 10, 12...]. The Hamming numbers are those
-- that only have 2, 3, and 5 as prime divisors. Use the function hammings::[Integer]
hammings :: [Integer]
hammings = 1 : filter isHamming (iterate (+ 1) 2)
  where
    isHamming :: Integer -> Bool
    isHamming n
      | n == 2 || n == 3 || n == 5 = True
      | n `mod` 2 == 0 = isHamming (n `div` 2)
      | n `mod` 3 == 0 = isHamming (n `div` 3)
      | n `mod` 5 == 0 = isHamming (n `div` 5)
      | otherwise = False

hammings' :: [Integer]
hammings' = 1 : merge (map (* 2) hammings') (merge (map (* 3) hammings') (map (* 5) hammings'))
  where
    merge :: [Integer] -> [Integer] -> [Integer]
    merge (x : xs) (y : ys)
      | x < y = x : merge xs (y : ys)
      | x == y = x : merge xs ys -- Filter out duplicates
      | otherwise = y : merge (x : xs) ys
