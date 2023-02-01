--1. Folosind numai metoda prin selectie definiti o functie
--astfel încât factori n întoarce lista divizorilor pozitivi ai lui n.
factori :: Int -> [Int]
factori x = [y | y <- [1..x], x `rem` y == 0]

factoriM x = do
    y <- [1..x]
    if x `rem` y == 0 then return y else []


--2. Folosind functia factori, definiti predicatul prim n care întoarce True dacă si numai dacă n este număr prim.
prim :: Int -> Bool
prim n = length (factori n) == 2

primM n = do
    if length (factoriM n) == 2 then return True else return False

--3. Folosind numai metoda prin selectie si functiile definite anterior,
--definiti functia astfel încât numerePrime n întoarce lista numerelor prime din intervalul [2..n].
numerePrime :: Int -> [Int]
numerePrime n = [x | x<-[2..n], prim x]

numerePrimeM n = do 
    let potentialPrimes = [2..n]
    primes <- filter prim potentialPrimes
    return primes

--4. Definiti functia myzip3 care se comportă asemenea lui zip dar are trei argumente:
myzip3 :: [a] -> [a] -> [a] -> [(a,a,a)]
myzip3 x y z =  [(a, b, c) | ((a, b), c) <- zip (zip x y) z]    


--monada

myzip3m l1 l2 l3 = do
    list <- zip (zip l1 l2) l3
    return list

-- 5. Scrieti o functie generică firstEl care are ca argument o listă de perechi 
-- de tip (a,b) si întoarce lista primelor elementelor din fiecare pereche:
-- firstEl [('a',3),('b',2), ('c',1)] => "abc"

firstEl :: [(a,b)] -> [a]
firstEl = map fst

--fst is a function in Haskell that returns the first element of a pair (also known as a tuple) // snd = second

firstElc l = do
    fe <- map fst l
    return fe

-- 6 .Scrieti functia sumList care are ca argument o listă de liste de valori
-- Int si întoarce lista sumelor elementelor din fiecare listă (suma elementelor
-- unei liste de întregi se calculează cu functia sum):

sumList :: [[Int]] -> [Int]
sumList = map sum

--sumListM :: Monad m => [[Int]] -> m [Int]
sumListM xs = do
  sums <- map sum xs
  return sums

-- 7. Scrieti o functie prel2 care are ca argument o listă de Int si
-- întoarce o listă în care elementele pare sunt înjumătătite, 
-- iar cele impare sunt dublate:

prel2 :: [Int] -> [Int]
prel2 = map (\x -> if odd x then 2 *x else x `div` 2)

--monada

prel2c l = do
    x <- l
    return $ if odd x then 2 * x else x `div` 2

-- 8. Scrieti o functie care primeste ca argument un caracter si o listă 
-- de siruri rezultatul fiind lista sirurilor care contin caracterul
-- respectiv (folositi functia elem).

functie :: Char -> [String] -> [String]
functie x = filter (x `elem`)

--monada

functiec l1 l2 = do
    e <- l2
    if l1 `elem` e then return e else []

-- 9. Scrieti o functie care primeste ca argument o listă de întregi si
-- întoarce lista pătratelor numerelor impare.

functie2 :: [Int] -> [Int]
functie2 l = map (\x -> if odd x then x * x else x) l

--monada

functie2c l = do
    x <- l
    return $ if odd x then x*x else x

-- 10.Scrieti o functie care primeste ca argument o listă de întregi 
-- si întoarce lista pătratelor numerelor din pozitii impare. 
-- Pentru a avea acces la pozitia elementelor folositi zip.

functie3 :: [Int] -> [Int]
functie3 l = map (\(a,b) -> a * a) (filter (odd . snd) (zip l [1..length l]))

--monada

functie3c :: [Int] -> [Int]
functie3c l = do
  (a, b) <- filter (odd . snd) (zip l [1..length l])
  return (a * a)

-- 11. Scrieti o functie care primeste ca argument o listă de siruri de 
-- caractere si întoarce lista obtinută prin eliminarea consoanelor din 
-- fiecare sir.
charToString :: Char -> String
charToString c = [c]

aux :: String -> String
aux "" = ""
aux (xs:s) =
    if xs `elem` "aeiouAEIOU"
        then charToString xs ++ aux s
        else aux s
numaiVocale :: [String] -> [String]
numaiVocale s = map (aux) s

--monada

aux2 "" = ""
aux2 (x:xs) = do
  if x `elem` "aeiouAEIOU"
    then charToString x ++ aux2 xs
    else aux2 xs

numaiVocale2 :: [String] -> [String]
numaiVocale2 s = do
  x <- s
  return $ aux2 x

-- 12. Definiti recursiv functiile mymap si myfilter cu aceeasi functionalitate
-- ca si functiile predefinite.

mymap :: (a -> b) -> [a] -> [b]
mymap fct [] = []
mymap fct (x:xs) = fct x : mymap fct xs

myfilter :: (a -> Bool) -> [a] -> [a]
myfilter fct [] = []
myfilter fct (x:xs) =
    if fct x
        then x : myfilter fct xs
        else myfilter fct xs

--monada

mymapM :: Monad m => (a -> m b) -> [a] -> m [b]
mymapM fct xs = sequence (map fct xs)

myfilterM :: Monad m => (a -> m Bool) -> [a] -> m [a]
myfilterM fct xs = do
    bools <- mapM fct xs
    return [x | (x, True) <- zip xs bools]