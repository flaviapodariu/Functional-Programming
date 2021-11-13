import Data.Char
import Data.List


-- 1.
rotate :: Int -> [Char] -> [Char]
rotate x str = (drop x str ++ take x str) 

--2.
prop_rotate :: Int -> String -> Bool
prop_rotate k str = rotate (l - m)(rotate m str) == str
                        where l = length str
                              m = if l == 0 then 0 else k `mod` l

-- -- 3.
makeKey :: Int -> [(Char, Char)]
makeKey x = zip str (rotate x str)
                where str = ['A'..'Z']
                 


-- -- 4.

lookUp :: Char -> [(Char, Char)] -> Char
lookUp c pairs
  | length [b | (a,b)<- pairs, a == c] > 0 = head [b | (a,b)<- pairs, a == c]
  | otherwise = c

-- -- 5.
encipher :: Int -> Char -> Char
encipher x c = lookUp c (makeKey x)

-- -- 6.
normalize :: String -> String
normalize str = (map toUpper)(filter isAlphaNum str)
-- -- 7.
encipherStr :: Int -> String -> String
encipherStr x str = (map (encipher x))(normalize str)
-- -- 8.
reverseKey :: [(Char, Char)] -> [(Char, Char)]
reverseKey pairs = [(b,a) | (a,b)<-pairs] 

-- -- 9.
decipher :: Int -> Char -> Char
decipher x c = lookUp c (reverseKey(makeKey x))

decipherStr :: Int -> String -> String
decipherStr x str = (map (decipher x) [c | c<-str, c == toUpper c, isAlphaNum c])
