eeny :: Integer -> String
eeny = undefined
fizzbuzz :: Integer -> String
fizzbuzz = undefined


fibonacciCazuri :: Integer -> Integer
fibonacciCazuri n
    | n < 2     = n
    | otherwise = fibonacciCazuri (n - 1) + fibonacciCazuri (n - 2)

fibonacciEcuational :: Integer -> Integer
fibonacciEcuational 0 = 0
fibonacciEcuational 1 = 1
fibonacciEcuational n =
    fibonacciEcuational (n - 1) + fibonacciEcuational (n - 2)


tribonacci :: Integer -> Integer
tribonacci 0 = 0
tribonacci 1 = 1
tribonacci 2 = 1
tribonacci 3 = 2
tribonacci n =
    tribonacci (n-1) + tribonacci (n-2) + tribonacci (n-3)


binomial :: Integer -> Integer -> Integer
binomial 0 k = 1
binomial n 0 = 1
binomial n k = 
    binomial (n-1) k + binomial (n-1) (k-1)


verifL :: [Int] -> Bool
verifL a =
    let l = length a in
    if even l then
        True
    else
        False


takefinal :: [Int] -> Int -> [Int]
takefinal a n = 
    let l = length a in
    if n >= l then 
        a
    else 
        drop (l-n) a


remove :: [Int] -> Int -> [Int]
remove a n = 
    let (b,c) = splitAt (n-1) a in b ++ (tail c)


-- semiPareRec [0,2,1,7,8,56,17,18] == [0,1,4,28,9]
semiPareRec :: [Int] -> [Int]
semiPareRec [] = []
semiPareRec (h:t)
 | even h    = h `div` 2 : t'
 | otherwise = t'
 where t' = semiPareRec t
    

sumImp :: [Int] -> Int
sumImp [] = 0
sumImp (h:t)
 | odd h    = h + s
 | otherwise = s
 where s = sumImp t


totalLen :: [String] -> Int
totalLen [] = 0
totalLen (h:t)
 | head h == 'A' = length h + s
 | otherwise = s
 where s = totalLen t


