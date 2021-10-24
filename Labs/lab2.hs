--1 
--poly2 :: Double -> Double -> Double -> Double -> Double
--poyl2 a b c x = a* (x**2) + b*x + cu

--2
eeny :: Integer -> String
eeny x = 
    if (even x)
        then "eeny"
        else "meeny"

--3 cu if

fizzbuzz :: Integer -> String
fizzbuzz x = 
    if (x `mod` 15 == 0)
       then "FizzBuzz"
       else 
          if(x `mod` 3 == 0)
             then "Buzz"
             else
                if(x `mod` 5 == 0)
                   then "Fizz"
                   else ""
--3 cu garzi

fizzbuzz2 :: Integer -> String
fizzbuzz2 x
    |mod x 15 == 0 = "FizzBuzz"
    |mod x 3 == 0 = "Buzz"
    |x `mod` 5 == 0 = "Fizz"
    |otherwise = ""

-- Recursivitate

--4 

tribonacci :: Integer -> Integer
tribonacci n
    |n == 1 = 1
    |n == 2 = 1
    |n == 3 = 2
    |otherwise = tribonacci(n-3) + tribonacci(n-2) + tribonacci(n-1)

--5 

binomial :: Integer -> Integer -> Integer
binomial n k 
    |k == 0 = 1
    |n == 0 = 0
    |otherwise = binomial(n-1) (k) + binomial(n-1) (k-1)


--6a
verifL :: [Int] -> Bool
verifL lista =
     if(even(length lista))
         then True
         else False

--6b
takefinal :: [Int] -> Int -> [Int]
takefinal lista n =
    if(length lista < n)
       then lista
       else drop (length lista - n) lista

--6c
remove :: [Int] -> Int -> [Int]
remove lista n = take(n-1) lista ++ drop(n) lista

myreplicate :: Int -> a -> [a]
myreplicate 0 _ = []
myreplicate n v = v : myreplicate (n-1) v

sumImp :: [Integer] -> Integer
sumImp [] = 0
sumImp (x : xs)
  | even x = sumImp xs
  | otherwise = x + sumImp xs

  
totalLen :: [String] -> Int
totalLen [] = 0
totalLen (a:b) = if (take 1 a == "A")
                    then (length a + totalLen b)
                    else totalLen b