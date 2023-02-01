import Data.List

myInt = 5555555555555555555555555555555555555555555555555555555555555555555555555555555555555

double :: Integer -> Integer
double x = x+x

triple :: Integer -> Integer
triple x = x+x+x

--maxim :: Integer -> Integer -> Integer
maxim x y = if (x > y)
               then x
          else y

max3 x y z = let
             u = maxim x y
             in (maxim  u z)

spatrate :: Integer -> Integer -> Integer
spatrate x y = x*x+y*y

paritate :: Integer -> String
paritate p = if (even p)
               then "par"
          else "impar"

factorial :: Integer -> Integer
factorial 0 = 1
factorial f = f * factorial(f-1)

compdublu :: Integer -> Integer -> String
compdublu x y = if (x > y*2)
                    then "true"
               else "false"