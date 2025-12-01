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
