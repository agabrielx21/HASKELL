getFromInterval a b list = do 
    x <- list 
    if a <= x && x <= b then return x else []


--2. Folosind functia factori, definiti predicatul prim n care întoarce True dacă si numai dacă n este număr prim.
factori :: Int -> [Int]
factori x = [y | y <- [1..x], x `rem` y == 0]

prim :: Int -> Bool
prim n = length (factori n) == 2

prim2 t = do
    if length(factori t) == 2 then return True else return False

--3. Folosind numai metoda prin selectie si functiile definite anterior,
--definiti functia astfel încât numerePrime n întoarce lista numerelor prime din intervalul [2..n].
numerePrime :: Int -> [Int]
numerePrime n = [x | x<-[2..n], prim x]

numerePrime2 x = do
    y <- [2..x]
    if prim y == True then return y else []

-------------
main :: IO ()
main = do
  putStrLn "Enter a line of text:"
  line <- getLine
  putStrLn ("You entered: " ++ line)
