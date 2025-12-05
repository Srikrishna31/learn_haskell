import Text.Read (readMaybe)

{-
Write a program that reads the name of the person and writes a nice message. The input is the name of the person.
If the name has an A, "You belong to Group A!" must be written. Otherwise, "You belong to Group B!" must be written.
Write a main action.
-}
-- main :: IO ()
-- main =
--   do
--     putStrLn "Enter your name:"
--     name <- getLine
--     if 'a' `isin` name || 'A' `isin` name
--       then
--         putStrLn "You belong to Group A!"
--       else
--         putStrLn "You belong to Group B!"

isin :: Char -> String -> Bool
isin c = foldl (\acc y -> acc || y == c) False

{-
    You are at a key point on a deserted road. Depending on the number of kilometers you are willing to travel, you have to go to one city or
    another. Following the table:

    KM                                          City
    < 18                                        Napa
    18 < x < 25                             DavenPort
    25 < x < 30                             Naperville
    30 < x < 45                             Phoenix
    > 45                                        Carbondale

    Input:
        Input is organized in lines. Each line has two elements separated with a whitespace: the name and the km. The last line special and
        only contains an asterisk.

    Output
        For each individual, print his/her name and the city to which he/she is addressed.
-}
main :: IO ()
main =
  do
    line <- getLine
    if line /= "*"
      then do
        -- putStrLn $ show (citySol line)
        print $ citySol line
        main
      else
        return ()

citySol :: String -> Either String String
citySol line = case city of
  Right cty -> Right $ name ++ ": " ++ cty
  Left cty -> Left cty
  where
    city = case km of
      Right kmv -> Right $ get_city kmv
      Left e -> Left e
    (name, km) = get_name_km line

    get_city :: Int -> String
    get_city km
      | km < 18 = "Napa"
      | 18 < km && km < 25 = "Davenport"
      | 25 < km && km < 30 = "Naperville"
      | 30 < km && km < 45 = "Phoenix"
      | otherwise = "Carbondale"

    get_name_km :: String -> (String, Either String Int)
    get_name_km line =
      (name, km)
      where
        -- (name, km_string) = (takeWhile (/= ' ') line, dropWhile (/= ' ') line)
        (name, km_string) = span (/= ' ') line
        km = case readMaybe km_string of
          Just km -> Right km
          Nothing -> Left "Could not convert km to integer"
