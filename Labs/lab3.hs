import Data.Char

--1
palindrom :: String -> Bool
palindrom str = 
   str == reverse str


nrVocale :: [String] -> Int
nrVocale [] = 0
nrVocale (c:s) = (if palindrom c then nrVoc c else 0) + nrVocale s
    where
    vocale = "aeiouAEIOU"
    nrVoc "" = 0
    nrVoc (c:s)
      |elem c vocale = 1 + nrVoc s
      |otherwise = nrVoc s 


-- --2
ex2 :: Int -> [Int] -> [Int]
ex2 x [] = []
ex2 x (c:s)
    | even x = c:x:ex2 x s
    | otherwise = c:ex2 x s
--3
semiPareComp :: [Int] -> [Int]
semiPareComp l = [ x `div` 2 | x <- l, even x ]

divizori :: Int -> [Int]
divizori x = [y | y <- [1..x], x `mod` y == 0]

--4
listadiv :: [Int] -> [[Int]]
listadiv lista =
    [ divizori(c) | c<-lista]

--5

inInterval :: Int -> Int -> [Int] -> [Int]
inInterval x y [] = []
inInterval x y lista = 
   [e | e <- [x..y], e `elem` lista]   

--6a

pozitiveRec :: [Int] -> Int
pozitiveRec [] = 0
pozitiveRec (s:f) = 
    (if s > 0 then 1 else 0) + pozitiveRec f

--6b

pozitiveComp :: [Int] -> Int
pozitiveComp [] = 0
pozitiveComp lista =
   let
     a = [x | x <- lista, x > 0] 
   in length a 

--7a
helpPos :: Int -> [Int] -> [Int]
helpPos pos [] = []
helpPos pos (s:f)
  | odd s = [pos] ++ helpPos (pos + 1) f
  | otherwise = helpPos (pos + 1) f

pozitiiImpareRec :: [Int] -> [Int]
pozitiiImpareRec lista = 
    helpPos 0 lista

--7b

pozitiiImpareComp :: [Int] -> [Int]
pozitiiImpareComp [] = []
pozitiiImpareComp lista =
    let
      indexes = zip [0..length lista - 1] lista
    in [x | (x,y) <- indexes, odd y]



--8a
multDigitsRec :: String -> Int 
multDigitsRec [] = 1
multDigitsRec (s:f)
  | isDigit s = (digitToInt s) * multDigitsRec f
  | otherwise = 1 * multDigitsRec f

--8b
multDigitsComp :: String -> Int 
multDigitsComp [] = 1
multDigitsComp lista = 
    let
     digits = [digitToInt x | x <- lista, isDigit x]
    in product digits 