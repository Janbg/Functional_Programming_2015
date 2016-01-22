removeAt :: Int -> [a] -> [a]
removeAt i xs
    | (i < 0) || (i >= length xs)  = error "Index out of bounds"
    | otherwise                    = take i xs ++ drop (i + 1) xs
