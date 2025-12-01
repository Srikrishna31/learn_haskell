f :: Int -> Maybe Int
f 0 = Nothing
f n = Just n

g :: Int -> Maybe Int
g 100 = Nothing
g n = Just n

k :: String -> Maybe String
k "Field" = Nothing
k "Sea" = Nothing
k s = Just s

secDiv :: Float -> Float -> Either String Float
secDiv _ 0 = Left "Division by 0"
secDiv x y = Right (x / y)

data Shape
  = Rectangle Float Float
  | Square Float
  | Circle Float
  | Point

area :: Shape -> Float
area (Rectangle width height) = width * height
area (Square side) = side * side
area (Circle radius) = pi * radius * radius
area Point = 0.0
