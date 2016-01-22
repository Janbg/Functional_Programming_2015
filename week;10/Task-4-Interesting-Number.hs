divisors :: Int -> [Int]
divisors n = 1 : filter ((==0) . rem n) [2 .. n `div` 2]

sumDivisors :: (Num a) => [a] -> a  
sumDivisors xs = foldl (\acc x -> acc + x) 0 xs  

dOFn :: Int -> Int
dOFn n = (sumDivisors (divisors n))


interestingNumber :: Int -> Bool
interestingNumber n =   n == (dOFn (dOFn n))