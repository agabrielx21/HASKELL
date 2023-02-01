-- 1.
sumImpare :: [Int] -> Int
sumImpare l = foldl (+) 0 (map(^2)(filter odd l))

-- 2.
verifTrue :: [Bool] -> Bool
verifTrue = foldr (&&) True

verifTrueM list = do
    return $ foldr (&&) True list
    -- let result = foldr .. list \n return result
-- 3.
allVerifies :: (Int -> Bool) -> [Int] -> Bool
allVerifies f l = foldl (&&) True (map f l)

-- 4.
anyVerifies :: (Int->Bool) -> [Int] -> Bool
anyVerifies f l = foldl (||) False (map f l)

-- 5.
mapFoldr :: (a -> b) -> [a] -> [b]
mapFoldr f xs = foldr (\x acc -> f x : acc) [] xs

filterFoldr :: (a -> Bool) -> [a] -> [a]
filterFoldr p xs = foldr (\x acc -> if p x then x : acc else acc) [] xs

-- 6.
listToInt :: [Integer] -> Integer
listToInt = foldl adauga 0
    where
        adauga num d = 10 * num + d

--monada--

listToIntM list = do
    return $ foldl adauga 0 list
    where adauga num d = 10 * num + d

-- 7. a)

rmChar :: Char -> String -> String
rmChar a s = filter (/= a) s

--monada--

rmCharM x y = do
    return $ filter (/= x) y
    
-- b)
rmCharsRec :: String -> String -> String
rmCharsRec [] b = b
rmCharsRec a [] = []
rmCharsRec (x:xa) b =
    if x `elem` b
        then rmCharsRec xa (rmChar x b)
        else rmCharsRec xa b


-- c)

rmCharsFold :: String -> String -> String
rmCharsFold a b = foldr rmChar b a