isPrime :: Int -> Bool
isPrime n
    | n <= 1 = False
    | otherwise = isPrime' 2 n
        where
            isPrime' current n
                | current == n = True
                | mod n current == 0 = False
                | otherwise = isPrime' (current + 1) n
                
                
truncatablePrime :: Int -> Bool
truncatablePrime x 
     | x < 10 = isPrime x
     | otherwise = (isPrime x) && (truncatablePrime (div x 10))