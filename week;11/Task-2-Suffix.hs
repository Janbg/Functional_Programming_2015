import Data.List

suffix :: (Eq a) => [a] -> [a] -> Bool
suffix [] [] = False
suffix  _ [] = False
suffix []  _ = False
suffix xs ys = xs == (drop ((length ys) - (length xs)) ys)
