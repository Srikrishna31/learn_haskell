import Data.Char
-- List Comprehensions

{-
Basic Concepts
In mathematics, the comprehension notation can be used to construct
new sets from existing sets. For example, the comprehension { x^2 | x E {1..5}}
produces the set {1, 4, 9, 16, 25} of all numbers x^2 such that x is 
an element of the set {1..5}. In Haskell a similar notation can be used 
to construct new lists from existing lists:
    > [x ^2 | x <- [1..5]]
    [1,4,9,16,25]

The symbol | is read as "such that", <- is read as "drawn from", and the
expression x <- [1..5] is called a generator. A list comprehension can 
have more than one gnerator, with successive generators being separated
by commas. eg:
    > [(x,y) | x <- [1,2,3], y <- [4,5]]
    [(1,4),(1,5),(2,4),(2,5),(3,4),(3,5)]
Changing the order of the two gnerators produces the following:
    > [(x,y) | y <- [4,5], x <- [1,2,3]]
    [(1,4),(2,4),(3,4),(1,5),(2,5),(3,5)]

These behaviors can be understood by thinking of later generators as being
more deeply nested, and hence changing the values of their variables more
frequently than earlier generators.

Later generators can also depend upon the values of variables from earlier
generators, as below:
    > [(x,y) | x <- [1..3], y <- [x..3]]
    [(1,1),(1,2),(1,3),(2,2),(2,3),(3,3)]

As a more practical example of this idea, the library function concat that
concatenates a list of lists can be defined by using one generator to select
each list in turn, and another to select each element from each list:
-}
concat :: [[a]] -> [a]
concat xss = [x | xs <- xss, x <- xs]

-- The wildcard pattern _ is sometimes useful in generators to discard
-- certain elements from a list.
firsts :: [(a,b)] -> [a]
firsts ps = [x | (x,_) <- ps]

-- In the following, the generator _ <- xs simply serves as a counter to
-- govern the production of the appropriate number of ones.
length :: [a] -> Int
length xs = sum [1 | _ <- xs]

{-
Guards

List comprehensions can also use logical expressions called guards to 
filter the values produced by earlier generators. If a guard is True,
then the current values are retained; if it is False, then they are 
discarded. 
-}
factors :: Int -> [Int]
factors n = [x | x <- [1..n], n `mod` x == 0]

prime :: Int -> Bool
prime n = factors n == [1,n]


primes :: Int -> [Int]
primes n = [x | x <- [2..n], prime x]


{-
Given a list of pairs of keys and values representing a lookup table,
we define a function find for any type of keys that supports equality,
that returns the list of all values that are associated with a given
key in a table is defined as below: 
-}
find :: Eq a => a -> [(a,b)] -> [b]
find k t = [v | (k', v) <- t, k == k']


{-
The zip function
The library function zip produces a new list by pairing successive
elements from two lists until either or both lists are exhausted.

The pairs function returns the list of all pairs of adjacent
elements from a list as below:
-}
pairs :: [a] -> [(a,a)]
pairs xs = zip xs (tail xs)


sorted :: Ord a => [a] -> Bool
sorted xs = and [x <=y | (x,y) <- pairs xs]


{-
Positions is a function that returns the list of all positions at
which a value occurs in a list, by pairing each element with its 
position, and selecting those positions at which the desired value
occurs.

Within the definition for positions, the expression [0..] produces
the list of indices [0,1,2,3,...]. This list is notionally infinite,
but under lazy evaluation only as many elements of the list as required
by the context in which it is used will actually be produced.
Exploiting lazy evaluation in this manner avoids the need to explicitly
produce a list of indices of the same length as the input list.
-}
positions :: Eq a => a -> [a] -> [Int]
positions x xs = [i | (x', i) <- zip xs [0..], x == x']

{-
String comprehensions
In Haskell, strings are constructed as list of characters. Because
they are lists, any polymorphic function on lists can also be used
with strings.
-}
lowers :: String -> Int
lowers xs = Prelude.length [x | x <- xs, isAsciiLower x]

count :: Char -> String -> Int
count x xs = Prelude.length [x' | x' <- xs, x == x']



{-
The Caesar Cipher
To encode a string, Caesar simply replaced each letter in the string 
by the letter three places further down in the alphabet, wrapping
around at the end of the alphabet.
More generally, the specific shift factor of three used by Caesar can
be replaced by any integer between one and twenty-five, there by
giving twenty-five different ways of encoding a string.
-}
let2int :: Char -> Int
let2int c = ord c - ord 'a'

int2let :: Int -> Char
int2let n = chr (ord 'a' + n)


shift :: Int -> Char -> Char
shift n c | isLower c = int2let ((let2int c + n) `mod` 26)
          | otherwise = c


{-
A separate function to decode a string is not required, because
this can be achieved by simply using a negative shift factor.
-}
encode :: Int -> String -> String
encode n xs = [shift n x | x <- xs]


{-
Frequency Tables
The key to cracking the Caesar cipher is the observation that some 
letters are used more frequently than others in English text. By 
analysing a large volume of such text, one can derive the following
table of approximate percentage frequencies of the twenty-six letters
of alphabet:
-}
table :: [Float]
table = [8.1, 1.5, 2.8, 4.2, 12.7, 2.2, 2.0, 6.1, 7.0,
         0.2, 0.8, 4.0, 2.4, 6.7, 7.5, 1.9, 0.1, 6.0,
         6.3, 9.0, 2.8, 1.0, 2.4, 0.2, 2.0, 0.1]


-- The library function fromIntegral :: Int -> Float converts an 
-- integer to a floating-point number.
percent :: Int -> Int -> Float
percent n m = (fromIntegral n / fromIntegral m) * 100

-- compute the frequency table for any given string
-- The use of the local definition n = lowers xs 
-- within freqs ensures that the number of lower-case
-- letters in the argument string is calculated once,
-- rather than each of the twenty-six times that this 
-- number is used within the list comprehension.
freqs :: String -> [Float]
freqs xs = [percent (count x xs) n | x <- ['a'..'z']]
          where n = lowers xs


{-
Cracking the cipher
A standard method for comparing a list of observed
frequencies os with a list of expected frequencies es
is the chi-square statistic, defined by the following 
summation in which n denotes the length of the two
lists, and xsi denotes the ith element of a list xs
counting from zero:

    Summation from 0 to n - 1 ((osi - esi) ^ 2 / esi)

The smaller the chi square value, the better the match
between the two frequency lists.
-}
chisqr :: [Float] -> [Float] -> Float
chisqr os es = sum [((o - e)^2)/e | (o,e) <- zip os es]

rotate :: Int -> [a] -> [a]
rotate n xs = drop n xs ++ take n xs

{-
Now suppose that we are given an encoded string, but 
not the shift factor that was used to encode it, and 
wish to determine this number in order that we can decode
this string. This can be achieved by producing the frequency
table of the encoded string, calculating the chi-square
statistic for each possible rotation of this table with
respect to the table of expected frequencies, and using
the position of the minimum chi-square value as the shift
factor.
-}
crack :: String -> String
crack xs = encode (-factor) xs
    where 
        factor = head (positions (minimum chitab) chitab)
        chitab = [chisqr (rotate n table') table | n <- [0..255]]
        table' = freqs xs