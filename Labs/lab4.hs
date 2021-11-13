--1
factori :: Int -> [Int]
factori n = 
    [x |x<-[1..n], n `mod` x == 0]


--2
prim :: Int -> Bool
prim n 
   |[x | x<- factori(n)] == [1,n] = True
   |otherwise = False

--3
numerePrime :: Int -> [Int]
numerePrime n = 
    [x | x<-[1..n], prim(x) == True]

--4
myzip3 :: [Int] -> [Int] -> [Int] -> [(Int, Int, Int)]
myzip3 a b c =
    [(x, y, z) | ((x, y),z)<-zip (zip a b) c]    

--5
ordonataNat :: [Int] -> Bool
ordonataNat [] = True
ordonataNat [x] = True
ordonataNat (x:xs) =
   and [x < y | (x, y) <- zip (x:xs) xs]

--6
ordonataNat1 :: [Int] -> Bool
ordonataNat1 [] = True
ordonataNat1 [x] = True
ordonataNat1 (x:xs) = (x < head xs) && ordonataNat1 xs

-- ordonataNat1 (x:xs) = and [x < head xs, ordonataNat1 xs]
-- ordonataNat1 (x:xs) = 
--   if x > head xs
--     then False
--     else orodonataNat1 xs


--7
ordonata :: [a] -> (a -> a -> Bool) -> Bool
ordonata [] _ = True
ordonata [x] _ = True
ordonata (x:xs) op = if  x `op` (head xs)
                         then ordonata xs op 
                         else False

--8
-- (*<*) :: (Integer, Integer) -> (Integer, Integer) -> Bool



--9
compuneList :: (b -> c) -> [(a -> b)] -> [(a -> c)]
compuneList lista1 lista2 = [(a `op2` b) `op1` c | op1 <-  lista1, op2 <- lista2]