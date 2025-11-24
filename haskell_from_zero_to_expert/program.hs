factorial :: Integer -> Integer
factorial 0 = 1
factorial n = n * factorial (n - 1)

double x = 2 * x

-- Basic Types
-- Int: Integers of 64 bits
-- Integer: Integers of arbitrary size
-- Booleans: True or False
--  Float: 32-bit floating point numbers
-- Double: 64-bit floating point numbers
-- Char: Characters
