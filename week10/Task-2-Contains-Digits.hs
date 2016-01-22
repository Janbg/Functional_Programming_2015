checkNumberIn :: Int -> Int -> Bool
checkNumberIn n x
     | n < 10    =   n == x
     | otherwise = ((mod n 10) == x) || (checkNumberIn (div n 10) x)



containsDigits :: Int -> Int -> Bool
containsDigits n m 
     | m < 10  =  (checkNumberIn n m)
     | otherwise = (checkNumberIn n (mod m 10)) && (containsDigits n (div m 10))