-- Higher Order Functions
import Data.Char
import Data.List

{-
Basic concepts
In Haskell, it is also permissible to define functions that take functions as 
arguments. 
Formally speaking, a function that takes a function as an argument or returns
a function as a result is called a higher-order function. In practice, however,
because the term curried already exists for returning functions as results, the
term higher-order is often just used for taking functions as arguments.

Using higher-order functions considerably increases the power of Haskell, by allowing
common programming patterns to be encapsulated as functions within the language
itself. More generally, higher-order functions can be used to define domain-specific
languages within Haskell.
-}
twice :: (a->a) -> a -> a
twice f x = f (f x)


{-
Processing Lists
The standard prelude defines a number of useful higher-order functions for processing
lists. 
-}
map :: (a -> b) -> [a] -> [b]
map f xs = [f x | x <- xs]


{-
There are three points to note about map.
First of all, it is a polymorphic function that can be applied to lists of any type,
as are most higher-order functions on lists.
Secondly, it can be applied to itself to process nested lists. For example, the 
function map (map (+)) increments each number in a list of lists of numbers as 
shown below:

    map (map (+1)) [[1,2,3], [4,5]]
    = {applying outer map}
    [map (+1) [1,2,3], map (+1) [4,5]]
    = {applying inner map}
    [[2,3,4], [5,6]]

And, finally, the function map can also be defined using recursion as below.
That is, applying a function to all elements of the empty list gives the empty list,
while for a non-empty list the function is simply applied to the head of the list,
and we then proceed to apply the function to all the elements of the tail. The 
original definition for map using a list comprehension is simpler, but the recursive
definition is preferable for reasoning purposes.
-}
map' :: (a->b) -> [a] -> [b]
map' _ [] = []
map' f (x:xs) =  f x : map' f xs


{-
Another useful higher-order library function is filter, which selects all elements
of a list that satisfy a predicate, where a predicate (or property) is a function
that returns a logical value.

The functions map and filter are often used together in programs, with filter being
used to select certain elements from a list, each of which is then transformed
using map.
-}
filter' :: (a -> Bool) -> [a] -> [a]
filter' _ [] = []
filter' p (x:xs) | p x = x : filter' p xs
                 | otherwise = filter' p xs

filter'' :: (a -> Bool) -> [a] -> [a]
filter'' p xs = [x | x <- xs, p x]


sumsqreven :: [Int] -> Int
sumsqreven xs = Prelude.sum (map' (\x -> x * x) (filter' even xs))


{-
The foldr function
Many functions that take a list as their argument can be defined using the following
simple pattern of recursion on lists:
f [] = v
f (x:xs) = x # f xs

That is, the function maps the empty list to a value v, and any non-empty list to an 
operator # applied to the head of the list and the result of recursively processing 
the tail. For example, a number of familiar library functions on lists can be defined
using this pattern of recursion:

    sum [] = 0
    sum (x:xs) = x + sum xs

    product [] = 1
    product (x:xs) = x * product xs

    or [] = False
    or (x:xs) = x || or xs

    and [] = True
    and (x:xs) = x && and xs

    The higher-order library function foldr (abbreviating fold right) encapsulates
    this pattern of recursion for defining functions on lists, with the operator #
    and the value v as arguments.
-}
sum :: Num a => [a] -> a
sum = foldr (+) 0

product :: Num a => [a] -> a
product = foldr (*) 1

or :: [Bool] -> Bool
or = foldr (||) False

and :: [Bool] -> Bool
and = foldr (&&) True

{-
There new definitions could also include explicit list arguments, as in
    sum xs = foldr (+) 0 xs
but we prefer the above definitions in which these arguments are made
implicit using partial application because they are simpler.
The foldr function itself can be defined using recursion:
-}
foldr' :: (a -> b -> b) -> b -> [a] -> b
foldr' _ v [] = v
foldr' f v (x:xs) = f x (foldr' f v xs)

{-
That is, the function foldr f v maps the empty list to the value v, and
any non-empty list to the function f applied to the head of the list and the
recursively processed tail. In practice, however, it is best to think of the 
behavior of foldr f v in a non-recursive manner, as simply replacing each
cons operator in a list by the function f, and the empty list at the end by
the value v.
For example, applying the function foldr (+) 0 to the list 1: (2: (3: []))
gives the result 1 + (2 + (3 + 0))

-}
length' :: [a] -> Int
length' = foldr (\_ n -> 1 + n) 0

reverse' :: [a] -> [a]
reverse' [] = []
reverse' (x:xs) = reverse' xs ++ [x]


reverse'' :: [a] -> [a]
reverse'' = foldr (\x t -> t ++ [x]) []

-- snoc is cons reversed to indicate that this function appends an element
-- to the end of the list.
snoc :: a -> [a] -> [a]
snoc x xs = xs ++ [x]

reverse''' :: [a] -> [a]
reverse''' (x:xs) = snoc x (reverse xs)

reverse'''' :: [a] -> [a]
reverse'''' = foldr snoc []

{-
It is worth noting that the name fold right reflects the use of an operator that
is assumed to associate to the right. For example, evaluating foldr (+) 0 [1,2,3]
gives the result 1+(2+(3+0)), in which the bracketing specifies that addition is
assumed to associate to the right. More generally, the behavior of foldr can be
summarized as follows:

foldr (#) v [x0, x1, ...,xn] = x0 # (x1 # (... (xn # v)...))
-}


{-
The foldl function
It is also possible to define recursive functions on lists using an operator that
is assumed to associate to the left. For example, the function sum can be
redefined in this manner by usingan auxiliary function sum' that takes an extra
argument v that is used to accumulate the final result:
-}
sum' :: Num a => [a] -> a
sum' = sum'' 0
        where sum'' v [] = v
              sum'' v (x:xs) = sum'' (v + x) xs

{-
For example:
    sum' [1,2,3]
    = {applying sum'}
     sum'' 0 [1,2,3]
    = {applying sum''}
     sum'' (0+1) [2,3]
    = {applying sum''}
     sum'' (0+1+2) [3]
    = {applying sum''}
     sum'' (0+1+2+3) []
    = {applying sum''}
     ((0+1)+2)+3
    = {applying +}
     6

The bracketing in this calculation specifies that addition is now assumed to 
associate to the left. In practice, however, the order of association does not
affect the value of the result in this case, because addition is associative.
That is, x+(y+z) = (x+y)+z for an y numbers x, y, and z.

Generalizing from the sum example, many functions on lists can be defined using
the following simple pattern of recursion:

f v [] = v
f v (x:xs) = f (v # x) xs

That is, the function maps the empty list to the accumulator value v, and any
non-empty list to the result of recursively processing the tail  using a new
accumulator value obtained by applying an operator # to the current value and
the head of the list. The higher-order library function foldl (abbreviating
fold left) encapsulates this pattern of recursion, with the operator # and the
accumulator v as arguments.
-}
reverse''''' :: [a] -> [a]
reverse''''' = foldl (\xs x -> x:xs) []

{-
When a function can be defined using both foldr and foldl, the choice of which
definition is preferable is usually made on grounds of efficiency and requires
careful consideration of the evaluation mechanism underlying Haskell.

The foldl function itself can be defined using recursion:
-}
foldl' :: (a -> b-> a) -> a -> [b] -> a
foldl' _ v [] = v
foldl' f v (x:xs) = Main.foldl' f (f v x) xs

{-
In practice, however, as with foldr, it is best to think of the behavior of
foldl in a non-recursive manner, in terms of an operator # that is assumed
to associate to the left, as summarized by the following equation:

foldl (#) v [x0,x1,...,xn] = (... ((v # x0) # x1)...) # xn
-}

{-
The composition operator
The higher-order  library operator . returns the composition of two functions
as a single function, and can be defined as follows:
-}
(.) :: (b->c) -> (a->b) -> (a->c)
f . g = \x -> f (g x)

{-
That is, f.g, which is read as f composed with g, is the function that takes
an argument x, applies the function g to this argument, and applies the function
f to the result. This operator could also be defined by (f . g) x = f (g x).
However, we prefer the above definition in which the x argument is shunted to 
the body of the definition using a lambda expression, because it makes explicit
the idea that composition returns a function as its result.

Composition can be used to simplify nested function applications, by reducing
parentheses and avoiding the need to explicitly refer to the initial argument.
-}
odd'' :: Integral a => a -> Bool
odd'' = not Main.. even

twice' :: (a -> a) -> a -> a
twice' f = f Main.. f

sumsqreven' :: [Int] -> Int
sumsqreven' = sum' Main.. map' (^2) Main.. filter even

{-
The last definition exploits the fact that composition is associative. That is
f.(g.h) = (f.g).h for any functions f, g, and h of the appropriate types.
Hence, in a composition of three or more functions, there is no need to include 
parentheses to indicate the order of association, because associativity ensures
that this does not affect the result.
Composition also has an identity, given by the identity function:
-}
id :: a -> a
id = \x -> x

{-
The identity function is often useful when reasoning about programs, and also 
provides a suitable starting point for a sequence of compositions.
For example, the composition of a list of functions can be defined as follows:
-}
compose :: [a->a] -> (a->a)
compose = foldr (Main..) Main.id


{-
Binary string transmitter
To simplify the definition of certain functions, we assume for the remainder
of this example that binary numbers are written in reverse order to normal.
Eg. 1101 would be written as 1011, with successive bits as we move to the
right increasing in weight by a factor of two:
1101 = (1*1) + (2*0) + (4*1) + (8*1)
-}


type Bit = Int

{-
A binary number, represented as a list of bits, can be converted into an 
integer by simply evaluating the appropriate weighted sum:
-}
bin2int :: [Bit] -> Int
bin2int bits = sum' [w*b | (w,b) <- zip weights bits]
                where weights = iterate (*2) 1

{-
The higher-order library functions iterate produces an infinite list by
applying a function an increasing number of times to a value:

iterate f x = [x, f x, f (f x), f (f (f x)), ...]

Hence the experssion iterate (*2) 1 in the definition of bin2int produces
the list of weights [1,2,4,8...], which is then used to compute the weighted
sum by means of a list comprehension.
-}
bin2int' :: [Bit] -> Int
bin2int' = foldr (\x y -> x + 2 * y) 0

{-
Now let us consider the opposite conversion, from a non-negative integer
into a binary number. This can be achieved by repeatedly dividing the
integer by two and taking the remainder, until the integer becomes zero.
-}
int2bin :: Int -> [Bit]
int2bin 0 = []
int2bin n = n `mod` 2 : int2bin (n `div` 2)

{-
We will ensure that all our binary numbers have the same length, in this
case eight bits, by using a function make8 that truncates or extends a
binary number as appropriate to make it precisely eight bits:
The library function repeat :: a -> [a] produces an infinite list of copies
of a value, but lazy evaluation ensures that only as many elements as 
required by the context will actually be produced.
-}
make8 :: [Bit] -> [Bit]
make8 bits = take 8 (bits ++ repeat 0)


{-
Transmission
We now define a function that encodes a string of characters as a list of
bits by converting each character into a Unicode number, converting each 
such number into an eight-bit binary number, and concatenating each of 
these numbers together to produce a list of bits.
-}
encode :: String -> [Bit]
encode = concat Main.. map' (make8 Main.. int2bin Main.. ord)

{-
To decode a list of bits produced using encode, we first define a function
chop8 that chops such a list up into eight-bit binary numbers
-}
chop8 :: [Bit] -> [[Bit]]
chop8 [] = []
chop8 bits = take 8 bits : chop8 (drop 8 bits)

{-
It is now easy to define a function that decodes a list of bits as a string
of characters by chopping the list up, and converting each resulting binary
number into a Unicode number and then a character
-}
decode :: [Bit] -> String
decode = map' (chr Main.. bin2int) Main.. chop8

{-
Finally, we define a function transmit that simulates the transmission of a
string of characters as a list of bits, using a perfect communication channel
that we model using the identity function:
-}
transmit :: String -> String
transmit = decode Main.. channel Main.. encode

channel :: [Bit] -> [Bit]
channel = Main.id

{-
For our second extended programming example, we consider two different algorithms
for deciding the winner in an election: the simple first past the post system,
and the more refined alternative vote system.

First past the post
In this system, each person has one vote, and the candidate with the largest
number of votes is declared the winner.

First of all, we define a function that counts the number of times that a given
value occurs in a list, for any type whose values can be compared for equality.
-}
votes :: [String]
votes = ["Red", "Blue", "Green", "Blue", "Blue", "Red"]

count :: Eq a => a -> [a] -> Int
count x = length Main.. filter (==x)

{-
In turn, the higher-order function filter can also be used to define a function
that removes duplicate values from a list:
-}
rmdups :: Eq a => [a] -> [a]
rmdups [] = []
rmdups (x:xs) = x : filter (/=x) (rmdups xs)

{-
The functions count and rmdups can then be combined using a list comprehension
to define a function that returns the result of a first-past-the-post election
in increasing order of the number of votes received:
-}
result :: Ord a => [a] -> [(Int, a)]
result vs = sort [(count v vs, v) | v <- rmdups vs]


winner :: Ord a => [a] -> a
winner = snd Main.. last Main.. result

{-
Alternative vote

In this voting system, each person can vote for as many or as few candidates as
they wish, listing them in preference order on their ballot (1st choice, 2nd choice,
and so on). To decide the winner, any empty ballots are first removed, then the 
candidate with the smallest number of 1st-choice votes is eliminated from the ballots,
and same process is repeated until only one candidate remains, who then declared
the winner
-}
ballots :: [[String]]
ballots = [["Red", "Green"],
            ["Blue"],
            ["Green", "Red", "Blue"],
            ["Blue", "Green", "Red"],
            ["Green"]]


rmempty :: Eq a => [[a]] -> [[a]]
rmempty = filter (/= [])

elim :: Eq a => a -> [[a]] -> [[a]]
elim x = Main.map (filter (/=x))


{-
Define a function that ranks the 1st-choice candidates in each ballot in increasing
order of the number of such votes that were received
-}
rank :: Ord a => [[a]] -> [a]
rank = Main.map snd Main.. result Main.. Main.map head

{-
With the winner' function we first remove empty ballots, then rank the remaining
1st-choice candidates in increasing order of votes. If only one such candidate
remains, they are the winner, otherwise we eliminate the candidate with the smallest
number of 1st-choice votes and repeat the process.

The case mechanism of Haskell that is used in the below definition allows pattern 
matching to be used in the body of a definition and is sometims useful for avoiding
the need to introduce an extra function definition just for the purposes of 
performing pattern matching.
-}
winner' :: Ord a => [[a]] -> a
winner' bs = case rank (rmempty bs) of
                [c] -> c
                (c:cs) -> winner' (elim c bs)

{-
Show how the list comprehension [f x | x <- xs, p x] can be re-expressed using the 
higher-order functions map and filter
-}
filterMap :: (a -> a) -> (a -> Bool) -> [a] -> [a]
filterMap f p = map' f Main.. filter' p


{-
Without looking at the definitions from the standard prelude, define the following
higher-order library functions on lists.

a. Decide if all elements of a list satisfy a prediate:
    all :: (a -> Bool) -> [Bool] -> Bool

b. Decide if any element of a list satisfies a predicate:
    any:: (a -> Bool) -> [Bool] -> Bool

c. Select elements from a list while they satisfy a predicate:
    takeWhile :: (a -> Bool) -> [a] -> [a]

d. Remove elements from a list while they satisfy a predicate:
    dropWhile :: (a -> Bool) -> [a] -> [a]
-}
all :: (a -> Bool) -> [Bool] -> Bool
all f = Main.foldl' (&&) True

any :: (a -> Bool) -> [Bool] -> Bool
any f = Main.foldl' (||) False

takeWhile :: (a -> Bool) -> [a] -> [a]
takeWhile _ [] = []
takeWhile f (x:xs) | f x = x : Main.takeWhile f xs
                   | otherwise = []

dropWhile :: (a -> Bool) -> [a] -> [a]
dropWhile _ [] = []
dropWhile f (x:xs) | f x = Main.dropWhile f xs
                   | otherwise = []



{-
Redefine the functions map f and filter p using foldr.
-}
map'' :: (a -> b) -> [a] -> [b]
map'' f = foldr (\x acc -> f x : acc) []

filter''' :: (a -> Bool) -> [a] -> [a]
filter''' p = foldr (\x acc -> if p x then x : acc else acc) []


{-
Uisng foldl, define a function dec2int :: [Int] -> Int that converts a decimal
number into an integer. For example:
    > dec2int [2,3,4,5]
    2345
-}
dec2int :: [Int] -> Int
dec2int = foldl (\acc x -> x + 10 * acc) 0


{-
Without looking at the definitions from the standard prelude, define the 
higher-order library function curry that converts a function on pairs into
a curried function, and, conversely, the function uncurry that converts a 
curried function with two arguments into a function on pairs.
-}
curry :: ((a,b) -> c) -> (a -> b -> c)
curry f = \x y -> f(x,y)

uncurry :: (a->b->c)-> ((a,b) ->c)
uncurry f = \(a,b) -> f a b


{-
A higher-order function unfold that encapsulates a simple pattern of recursion
for producing a list can be defined as follows:
That is, the function unfold p h t produces the empty list if the predicate p
is true of the argument value, and otherwise produces a non-empty list by applying
the function h to this value to give the head, and the function t to generate 
another argument that is recursively processed in the same way to produce the tail
of the list. For example, the function int2bin can be rewritten more compactly
using unfold as shown below:
-}
unfold :: (t -> Bool) -> (t -> a) -> (t -> t) -> t -> [a]
unfold p h t x | p x = []
               | otherwise = h x : unfold p h t (t x)

int2bin' :: Int -> [Bit]
int2bin' = unfold (== 0) (`mod` 2) (`div` 2)

-- Redefine the functions chop8, map f and iterate f using unfold.
chop :: Int -> [Bit] -> [[Bit]]
chop n = unfold null (take n) (drop n)

chop8' :: [Bit] -> [[Bit]]
chop8' = chop 8

map'''' :: (a -> b) -> [a] -> [b]
map'''' f = unfold null (f Main.. head) tail 

iterate' :: (a -> a) -> a -> [a]
iterate' = unfold (const False) Main.id


{-
Modify the binary string transmitter example to detect simple transmission errors
using the concept of parity bits. That is, each eight-bit binary number produced
during encoding is extended with a parity bit, set to one if the number contains
an odd number of ones, and to zero otherwise. In turn, each resulting nine-bit binary
number consumed during decoding is checed to ensure that its parity bit is correct, with
the parity bit being discarded if this is the case, and a parity error being
reported otherwise.

Hint: the library function error:: String -> a displays the given string as an 
error message and terminates the program; the polymorphic result type ensures that
error can be used in any context.
-}
parity :: [Bit] -> Bit
parity xs | even (count 1 xs `mod` 2)  = 0
          | otherwise = 1

make8parity :: [Bit] -> [Bit]
make8parity bits = parity bs : bs
                        where bs = take 8 (bits ++ repeat 0)

encode' :: String -> [Bit]
encode' = concat Main.. map' (make8parity Main.. int2bin Main.. ord)

chop8parity :: [Bit] -> [[Bit]]
chop8parity = chop 9

checkParity :: [Bit] -> Bool
checkParity bs = head bs == (parity Main.. tail) bs

bin2int'' :: [Bit] -> Int
bin2int'' bs | parity (tail bs) == head bs = (bin2int Main.. tail) bs
            | otherwise = error "Parity Error"  

decodeWithParity :: [Bit] -> String
decodeWithParity = map' (chr Main.. bin2int'') Main.. chop8parity


{-
Test your new string transmitter program from the previous exercise using
a faulty communication channel that forgets the first bit, which can be 
modelled using the tail function on lists of bits.
-}
channelFaulty :: [Bit] -> [Bit]
channelFaulty = tail

transmitFaulty ::String -> String
transmitFaulty = decodeWithParity Main.. channelFaulty Main.. encode'

transmitCorrect ::String -> String
transmitCorrect = decodeWithParity Main.. channel Main.. encode' 

{-
Define a function altMap :: (a->b) -> (a->b) -> [a] -> [b] that
alternately applies its two argument functions to successive
elements in a list in turn about order. For example:
    > altMap (+10) (+100) [0, 1, 2, 3, 4]
    [10, 101, 12, 103, 14]
-}
altMap :: (a->b) -> (a->b) -> [a] -> [b]
altMap _ _ [] = []
altMap f g [x,y] = [f x , g y] 
altMap f _ [x] = [f x]
altMap f g (x:xs) = f x : altMap g f xs

{-
Using altMap, define a function luhn:: [Int] -> Bool that implements the
Luhn algorithm from the exercises in chapter 4 for bank card numbers of 
any length. Test your new function using your own bank card.
-}
luhnDouble :: Int -> Int
luhnDouble b = if c < 9 then c else c - 9
                where c = 2*b

luhn :: [Int] -> Bool
luhn xs = Main.sum (altMap luhnDouble Main.id xs) `mod` 10 == 0
