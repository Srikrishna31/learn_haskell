{-
    A `parser` is a program that takes a string of characters as input, and produces some form of tree that makes the
    syntactic structure of the string explicit.
    Parsers are an important topic in computing, because most real-life programs use a parser to preprocess their
    input. For example, a calculator program parses numeric expressions prior to evaluating them, while the GHC system
    parses Haskell programs prior to executing them. In each case, making the structure of the input explicit considerably
    simplifies its further processing.

    A parser can be naturally viewed directly as a function that takes a string and produces a tree.
        type Parser = String -> Tree
    for a suitable definition of Tree.

    In general, however, a parser might not always consume its entire argument string. So, the type for parsers will also
    return any unconsumed part of the argument string:

        type Parser = String -> (Tree, String)

    Similarly, a parser might not always succeed. To handle this, we further generalise our type for parsers to return a list of
    results, with the convention that the empty list denotes failure, and a singleton list denotes success:

        type Parser = String -> [(Tree, String)]

    Finally ,different parsers will likely return different kinds of trees, or more generally, any kind of value.

        type Parser = String -> [(a, String)]
-}
{-# LANGUAGE LambdaCase #-}

import Control.Applicative
import Data.Char
import System.IO (hFlush, hSetEcho, stdin, stdout)

-- To allow the Parser type to be made into instances of classes, it is first redefined using newtype, with a dummy constructor P.
newtype Parser a = P (String -> [(a, String)])

-- Parser of this type can be applied to an input string using a function that simply removes the dummy constructor:
parse :: Parser a -> String -> [(a, String)]
parse (P p) = p

{-
The first parsing primitive is called `item`, which fails if input string is empty, and succeeds with the first character as the
result value otherwise:
-}
item :: Parser Char
item =
  -- P
  --   ( \inp -> case inp of
  --       [] -> []
  --       (x : xs) -> [(x, xs)]
  --   )
  P
    ( \case
        [] -> []
        (x : xs) -> [(x, xs)]
    )

{-
        Sequencing Parsers

    We now make the parser type into an instance of the functor, applicative and monad classes, in order that the do
    notation can then be used to combine parsers in sequence.
-}
instance Functor Parser where
  -- fmap :: (a -> b) -> P a -> P b
  fmap f p =
    P
      ( \inp -> case parse p inp of
          [] -> []
          [(v, out)] -> [(f v, out)]
      )

{-
    That is, fmap applies a function to the result value of a parser if the parser succeeds, and propagates the failure otherwise.
        > parse (fmap toUpper item) "abc"
        [('A', "bc")]

        > parse (toUpper <$> item) "abc"
        [('A', "bc")]

        > parse (toUpper <$> item) ""
        []
-}
instance Applicative Parser where
  --     -- pure :: a -> P a
  pure v = P (\inp -> [(v, inp)])

  -- <*> :: P (a -> b) -> P a -> P b
  pg <*> px =
    P
      ( \inp -> case parse pg inp of
          [] -> []
          [(g, rem)] -> case parse px rem of -- this part can be replaced with fmap as well.
            [] -> []
            [(x, xs)] -> [(g x, xs)]
      )

{-
    In this case, pure transforms a value into a parser that always succeeds with this value as its result, without consuming
    any of the input string.

    In turn, <*> applies a parser that returns a function to a parser that returns an argument to give a parser that returns
    the result of applying the function to the argument, and only succeeds if all the components succeed.
-}

three :: Parser (Char, Char)
three = pure g <*> item <*> item <*> item
  where
    g :: a -> b -> c -> (a, c)
    g x _ z = (x, z)

instance Monad Parser where
  -- (>>=) :: P a -> (a -> P b) -> P b
  px >>= f =
    P
      ( \inp -> case parse px inp of
          [] -> []
          [(v, out)] -> parse (f v) out
      )

{-
    That is, the parser p >>= f fails if the application of the parser p to the input string inp fails, and otherwise applies the
    function f to the result value v to give another parser f v, which is then applied to the output string out that was produced
    by the first parser to give the final result.
-}

three' :: Parser (Char, Char)
three' = do
  x <- item
  item
  z <- item
  return (x, z)

{-
        Making Choices

   The do notation combines parsers in sequence, with the output string from each parser in the sequence becoming the
   input string for the next. Another natural way of combining parsers is to apply one parser to the input string and if this
   fails to then apply another to the same input instead.

    Making a choice between two alternatives isn't specific to parsers, but can be generalized to a range of applicative
    types.

        class Applicative f => Alternative f where
            empty :: f a
            (<|>) :: f a -> f a -> f a

    That is, for an applicative functor to be an instance of the Alternative class, it must support empty and <|> primitives
    of the specified types. The intuition is that empty represents the alternative that has failed, and <|> is an appropriate
    choice operator for the type. The two primitives are also required to satisfy the following identity and associativity laws:

        empty <|> x = x

        x <|> empty = x

        x <|> (y <|> z) = (x <|> y) <|> z

        instance Alternative Maybe where
            -- empty :: Maybe a
            empty = Nothing

           -- (<|>) :: Maybe a -> Maybe a -> Maybe a
           Nothing <|> my = my
           (Just x) <|> _ Just x

    The instance for the Parser type is a natural extension of this idea, where empty is the parser that always fails
    regardless of the input strin, and <|> is a choice operator that returns the result of the first parser if it succeeds
    on the input, and applies the second parser to the same input otherwise:
-}
instance Alternative Parser where
  -- empty :: Parser a
  empty = P (\inp -> [])

  -- (<|>) :: Parser a -> Parser a -> Parser a
  pa <|> pb =
    P
      ( \inp -> case parse pa inp of
          [] -> parse pb inp
          [(x, out)] -> [(x, out)]
      )

{-
    For eg:
        > parse empty "abc"
        []

        > parse (item <|> return 'd') "abc"
        [('a', "bc")]

        > parse (empty <|> return 'd') "abc"
        ['d', "abc"]
-}

sat :: (Char -> Bool) -> Parser Char
sat p = do
  x <- item
  if p x then return x else empty

digit :: Parser Char
digit = sat isDigit

lower :: Parser Char
lower = sat isLower

upper :: Parser Char
upper = sat isUpper

letter :: Parser Char
letter = sat isAlpha

alphanum :: Parser Char
alphanum = sat isAlphaNum

char :: Char -> Parser Char
char c = sat (== c)

{-
Using char, we can define a parser string xs for the string of characters xs, with the string itself returned as the result value.
The empty string can always be parsed, while for a non-empty string we parse the first character, recursively parse the
remaining characters, and return the string as the result value. Note that `string` only succeeds if the entire target string is
consumed from the input:
    > parse (string "abc") "abcdef"
    [("abc", "def")]

    > parse (string "abc") "ab1234"
    []
-}
string :: String -> Parser String
string [] = return []
string (x : xs) = do
  char x
  string xs
  return (x : xs)

{-
The next two parsers, many p and some p apply a parser p as many times as possible until it fails, with the result values
from each successful application of p being returned in a list. The difference between these two repetition primitives is
that many permit zero or more applications of p, whereas some  requires atleast one successful application.
Infact, there is no need to define many and some ourselves, as suitable default definitions are already provided in the
Alternative class:

    class Applicative f => Alternative f where
        empty :: f a
        (<|>) :: f a -> f a -> f a
        many :: f a -> f [a]
        some :: f a -> f [a]

        many x = some x <|> pure []
        some x = pure (:) <*> x <*> many x

Note that the two new functions are defined using mutual recursion. The above definition for many x states that x can
either be applied at least once or not at all, while the definition for some x states that x can be applied once and then
zero or more times, with the results being returned in a list. These functions are provided for any applicative type that
is an instance of the Alternative class, but are primarily intended for use with parsers.
-}

-- a parser for parsing variables (starting with lower case letter, and followed by any number of alphanumeric characters)
ident :: Parser String
ident = do
  x <- lower
  xs <- many alphanum
  return (x : xs)

nat :: Parser Int
nat = do
  xs <- some digit
  return (read xs)

space :: Parser ()
space = do
  many (sat isSpace)
  return ()

int :: Parser Int
int =
  do
    char '-'
    n <- nat
    return (-n)
    <|> nat

flt :: Parser Float
flt =
  do
    char '-'
    f <- flt
    return (-f)
    <|> posFloat
  where
    posFloat :: Parser Float
    posFloat = do
      xs <- some digit
      char '.'
      ds <- some digit
      return (read (xs ++ ['.'] ++ ds))

-- ignore the space around the token while parsing.
token :: Parser a -> Parser a
token p = do
  space
  v <- p
  space
  return v

identifier :: Parser String
identifier = token ident

natural :: Parser Int
natural = token nat

float :: Parser Float
float = token flt

symbol :: String -> Parser String
symbol xs = token (string xs)

{-
The below parser states that a list begins with an opening square bracket and a natural number, followed by zero or
more commas and natural numbers, and concludes with a closing square bracket.
    > parse nats " [1,2,3] "
    [([1,2,3], "")]
    > parse nats "[1,2,]"
    []
-}
nats :: Parser [Int]
nats = do
  symbol "["
  n <- natural
  ns <-
    many
      ( do
          symbol ","
          natural
      )
  symbol "]"
  return (n : ns)

{-
            Arithmetic Expressions
    The syntactic structure of a language can be formalized using the mathematical notion of a grammar, which is a
    set of rules that describes how strings of the language can be constructed.

            -> expr ::= expr + expr | expr * expr | ( expr ) | nat

            -> nat ::= 0 | 1 | 2 | ...

    The first rule states that an expression is either the addition or multiplication of two expressions, a parenthesised
    expression, or a natural number. In turn, the second rule states that a natural number is either zero, one, two etc.
    The problem with above grammar is that, it doesn't higher priority forf multiplication. It can be addressed by having a
    separate rule for each level of priority, with addition at the lowest level of priority, multiplication at middle level and
    parenthesis and numbers at the highest level:

            -> expr ::= expr + expr | term

            -> term ::= term * term | factor

            -> factor ::= (expr) | nat

            -> nat ::= 0 | 1 | 2 ...

    With above rules, the priority has been addressed, but associativity is still not addressed, multiplication and addition
    associate to the right. This can be easily rectified by modifying the rules for addition and multiplication to be
    recursive in their right argument only, rather than in both arguments:

            -> expr ::= term + expr | term

            -> term ::= factor * term | factor

    Now the grammar is unambiguous, in the sense that every well-formed expression has precisely one parse tree.
    The final modification is simplification of the grammar rules:

            -> expr ::= term (+ expr | E)

            -> term ::= factor (* term | E)

            -> factor ::= (expr) | nat

            -> nat ::= 0 | 1 | 2 ...
-}

expr :: Parser Int
expr =
  do
    t <- term
    do
      symbol "+"
      e <- expr
      return (t + e)
      <|> return t

term :: Parser Int
term =
  do
    f <- factor
    do
      symbol "*"
      t <- term
      return (f * t)
      <|> return f

factor :: Parser Int
factor =
  do
    symbol "("
    e <- expr
    symbol ")"
    return e
    <|> natural

{-
    > eval "2*3+4"
    10

    > eval "2*(3+4)"
    14

    > eval "2*3^4"
    *** Exception: Unused input ^4

    > eval "one plus two"
    *** Exception: Invalid input
-}
eval :: String -> Int
eval xs = case parse expr xs of
  [(n, [])] -> n
  [(_, out)] -> error ("Unused input " ++ out)
  [] -> error "Invalid input"

{-
            Calculator

            The first four buttons: q, c, d and = allow the user to quit, clear the display, delete a character, and evaluate an
            expression.
-}
box :: [String]
box =
  [ "+-----------------+",
    "|                           |",
    "+---+---+---+---+",
    "|  q  |  c  |  d  |  =  |",
    "+---+---+---+---+",
    "|  1  |  2  |  3  |  +  |",
    "+---+---+---+---+",
    "|  4  |  5  |  6  |  -  |",
    "+---+---+---+---+",
    "|  7  |  8  |  9  |  *  |",
    "+---+---+---+---+",
    "|  0  |  (  |  )  |  /  |",
    "+---+---+---+---+"
  ]

{-
    We define the buttons on the calculator as a list of characters, comprising both the twenty standard buttons that
    appear on the box itself, together with a number of extra characters that will be allowed for flexibility, namely Q, C, D,
    space, escape, backspace, delete and newline:
-}
buttons :: String
buttons = standard ++ extra
  where
    standard = "qcd=123+456-789*0()/"
    extra = "QCD \ESC\BS\DEL\n"

type Pos = (Int, Int)

-- Function to display a string at a given position on the screen.
writeat :: Pos -> String -> IO ()
writeat p xs =
  do
    goto p
    Prelude.putStr xs

goto :: Pos -> IO ()
goto (x, y) = Prelude.putStr ("\ESC[" ++ show y ++ ";" ++ show x ++ "H")

showbox :: IO ()
showbox = sequence_ [writeat (1, y) b | (y, b) <- zip [1 ..] box]

cls :: IO ()
cls = Prelude.putStr "\ESC[2J"

getCh :: IO Char
getCh = do
  hSetEcho stdin False
  x <- getChar
  hSetEcho stdin True
  return x

{-
The display function shows a string in the display of the calculator, by first clearing the display and then showing the last
thirteen characters of the string:
In this manner, if the user deletes characters from the string, they will automatically be removed from the display, and if
the user types more than thirteen characters, the display will appear to scroll to the left.
-}
display xs = do
  writeat (3, 2) (replicate 13 ' ')
  writeat (3, 2) (reverse (take 13 (reverse xs)))

{-
The calculator itself is controlled by a function calc that displays the current string, and then reads a character from the
keyboard without echoing it. If this character is a valid button, then it is processed, otherwise we sound a beep to indicate
an error and continue with the same string:
-}
calc :: String -> IO ()
calc xs = do
  display xs
  c <- getCh
  if c `elem` buttons
    then
      process c xs
    else do
      beep ([c] ++ " is unsupported option. Please choose from these: \n" ++ buttons)
      calc xs

beep :: String -> IO ()
beep s = do
  writeat (20, 1) (s ++ "\n \BEL")
  hFlush stdout
  return ()

process :: Char -> String -> IO ()
process c xs
  | c `elem` "qQ\ESC" = quit
  | c `elem` "dD\BS\DEL" = delete xs
  | c `elem` "=\n" = eval' xs
  | c `elem` "cC" = clear
  | otherwise = press c xs

quit :: IO ()
quit = goto (1, 14)

delete :: String -> IO ()
delete [] = calc []
delete xs = calc (init xs)

eval' :: String -> IO ()
eval' xs = case parse expr xs of
  [(n, [])] -> calc (show n)
  [(_, rs)] -> do
    beep ("Error in expression before this point: " ++ rs)
    calc xs

clear :: IO ()
clear = calc []

press :: Char -> String -> IO ()
press c xs = calc (xs ++ [c])

run :: IO ()
run = do
  cls
  showbox
  clear

{-
    Define a parser comment :: Parser () for ordinary Haskell comments that begin with the symbol -- and
    extend to the end of the current line, which is represented by the control character '\n'
-}
comment :: Parser ()
comment = do
  token (string "--")
  many alphanum
  char '\n'
  return ()

comment' :: Parser ()
comment' = do
  string "--"
  many (sat (/= '\n'))
  return ()

{-
Explain why the final simplification of the grammar for arithmetic expressions has a dramatic effect on the efficiency of
the resulting parser. Hint: begin by considering how an expression comprising a single number would be parsed if this
simplification step had not been made.

Answer from Book: Without left-factorizing, the resulting parser would backtrack excessively and take exponential time
in the size of the expression. For example, a number would be parsed four times before being recognized as an expression.
-}

{-
Define a suitable type Expr for arithmetic expressions and modify the parser for expressions to have type expr:: Parser Expr
-}
data Expr = Add Expr Expr | Mul Expr Expr | Paren Expr | Number Int

instance Show Expr where
  show (Add l r) = (show l) ++ " + " ++ (show r)
  show (Mul l r) = (show l) ++ " * " ++ (show r)
  show (Paren e) = "( " ++ show e ++ " )"
  show (Number n) = show n

expr' :: Parser Expr
expr' =
  do
    t <- term'
    do
      symbol "+"
      e <- expr'
      return (Add t e)
      <|> return t

term' :: Parser Expr
term' =
  do
    f <- factor'
    do
      symbol "*"
      t <- term'
      return (Mul f t)
      <|> return f

factor' :: Parser Expr
factor' =
  do
    symbol "("
    e <- expr'
    symbol ")"
    return (Paren e)
    <|> natural'

natural' :: Parser Expr
natural' = do
  n <- natural
  return (Number n)

{-
Extend the parser expr:: Parser Int to support subtraction and division, and to use integer values rather than natural
numbers, based upon the following revisions to the grammar:

    -> expr ::= term (+expr | - expr | empty)

    -> term ::= factor (* term | / term | empty)

    -> factor ::= (expr) | int

    -> int ::= ... | -1 | 0 | 1 | ...
-}
expr'' :: Parser Int
expr'' =
  do
    t <- term''
    do
      s <- symbol "+" <|> symbol "-"
      e <- expr''
      if s == "-" then return (t - e) else return (t + e)
      <|> return t

term'' :: Parser Int
term'' =
  do
    f <- factor''
    do
      s <- symbol "*" <|> symbol "/"
      t <- term''
      if s == "*" then return (f * t) else return (f `div` t)
      <|> return f

factor'' :: Parser Int
factor'' =
  do
    symbol "("
    e <- expr''
    symbol ")"
    return e
    <|> int

{-
    Further extend the grammar and parser for arithmetic expressions to support exponentiation ^, which is
    assumed to associate to the right and have higher priority than multiplication and division, but lower priority
    than parentheses and numbers. For example 2^3*4 means (2^3)*4.
    Hint: the new level of priority requires a new rule in the grammar.

        -> expr ::= exp (^ expr | empty)

        -> exp ::= term (+ expr | - expr | empty)

        -> term ::= factor (* term | / term | empty)

        -> factor ::= (expr) | int

        -> int ::= ... | -1 | 0 | 1 | ...
-}
expr''' :: Parser Int
expr''' =
  do
    t <- Main.exp
    do
      symbol "^"
      e <- expr'''
      return (t ^ e)
      <|> return t

exp :: Parser Int
exp =
  do
    t <- term'''
    do
      s <- symbol "+" <|> symbol "-"
      e <- expr'''
      if s == "-" then return (t - e) else return (t + e)
      <|> return t

term''' :: Parser Int
term''' =
  do
    f <- factor'''
    do
      s <- symbol "*" <|> symbol "/"
      t <- term'''
      if s == "*" then return (f * t) else return (f `div` t)
      <|> return f

factor''' :: Parser Int
factor''' =
  do
    symbol "("
    e <- expr'''
    symbol ")"
    return e
    <|> int

{-
Consider expressions built up from natural numbers using a subtraction opoeator that is assumed to
associate to the left.
    a. Translate this description directly into a grammar

    b. Implement this grammar as a parser expr :: Parser Int

    c. What is the problem with this parser?

    d. Show how it can be fixed. Hint: rewrite the parser using the repetition primitive many and the
    library function foldl.
-}

{-
    Modify the calculator program to indicate the approximate position of an error rather than just
    sounding a beep, by using the fact that the parser returns the unconsumed part of the input
    string.
-}
