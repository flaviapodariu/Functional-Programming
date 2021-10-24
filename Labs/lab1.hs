import Data.List 
myInt = 55
double :: Integer -> Integer
double x = x+x

-- am scris functia double
-- pt a iesi din Prelude si a rula codul scris aici trebuie sa fac load la fisier (:l  fisier.hs)

triple :: Integer -> Integer
triple x = x+x+x

-- am adaugat functia triple dar nu e vizibila in cmd => fac reload cu :r

-- Pt a reveni in  Prelude folosesc comanda :m - nume_modul

-- Fisierul lab1.hs contine un modul denumit automat 'Main'

-- pentru a opri rularea => Ctrl + C


func6a :: Double -> Double -> Double
func6a x y = x**2 + y**2


func6b :: Integer -> [Char]
func6b x = 
    if (x `mod` 2 == 0)
	    then "par"
	    else "impar"

func6c :: Integer -> Integer
func6c 0 = 1
func6c x = product [1..x]
    
func6d :: Integer -> Integer -> Bool
func6d x y = 
    if (x > y + y)
    	then True
    	else False
