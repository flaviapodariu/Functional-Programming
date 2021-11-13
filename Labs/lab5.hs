--1
first :: (a,b) -> a
first (a,b) = a

firstEl :: [(a,b)] -> [a]
firstEl lista = map first lista

--2
sumList :: [[Int]] -> [Int]
sumList lista = map sum lista

--3
helper :: Int -> Int
helper x
 |odd x = x*2
 |even x = x `div`2

prel2 :: [Int] -> [Int]
prel2 lista = map helper lista

--4

funcL :: Char -> [String] -> [String]
funcL litera lista = filter (litera `elem`) lista

--5
patrateImpare :: [Int] -> [Int]
patrateImpare lst = map(\x -> x^2)(filter odd lst)

--6
piPat :: [Integer] -> [Integer]
piPat l = map (\(a,b) -> b^2) (filter (\(a,b) -> odd a) (zip [0..] l))

--7

vocala :: Char -> Bool
vocala litera = litera `elem` "aeiouAEIOU"
numaiVocale :: [String] -> [String]
numaiVocale lst = map(filter vocala) lst

--8
myMap :: (a -> b) -> [a] -> [b]
myMap f [] = []
myMap f (x:xs) =  (f x) : myMap f xs

myFilter :: (a -> Bool) -> [a] -> [a]
myFilter f [] = []
myFilter f (x:xs)
  | f x = x: myFilter f xs
  | otherwise = myFilter f xs

--9
sumPatr :: [Int] -> Int
sumPatr lst = sum(map(\x -> x^2)(filter odd lst))

--10
ex10 :: [Bool] -> Bool
ex10 lst = foldr (&&) True lst
--11

rmChar :: Char -> String -> String