listToNumber :: [Integer] -> Integer
listToNumber = read . concatMap show