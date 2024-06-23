
double x = x + x

quadruple x = double (double x)

factorial n = product [1..n]

-- # In general, any function with two arguments can be written between its
-- # arguments by enclosing the name of the function in single back quotes ` `
average ns = sum ns `div` length ns


n =  a `div` length xs
    where
        a = 10
        xs = [1,2,3,4,5]

add :: (Int, Int) -> Int
add (x, y) = x + y

zeroto :: Int -> [Int]
zeroto n = [0..n]