module ListasBasicas where

import           Data.Function

-- -b +- sqrt(b^2 - 4ac) / 2a
-- Si b^2 == 4ac -> 1 solucion
-- Si b^2 < 4ac -> 0 solucion
-- Si b^2 > 4ac -> 2 soluciones

conjugate :: Num a => a -> a -> (a, a)
conjugate a b = (a + b, a - b)

raices :: (Int, Int, Int) -> Maybe (Float, Float)
raices (a, b, c)
    | det < 0   = Nothing
    | otherwise = Just (conjugate partB partD)
    where div2F = on (/) fromIntegral
          det   = b^2 - 4*a*c
          partB = div2F (-b) (2*a)
          partD = sqrt (div2F det ((2*a)^2))

factores :: Int -> [Int]
factores x = [n | n <- [1..(div x 2)], mod x n == 0]
-- factores x = filter (\n -> div x n * n == x) [1..(div x 2)]
-- factores x = [n | n <- [1..(div x 2)], div x n * n == x]

perfectos :: Int -> [Int]
perfectos x = [n | n <- [6..x], sum (factores n) == n]

abundantes :: Int -> [Int]
abundantes x = [n | n <- [1..x], sum (factores n) > n]

paresNenCirculo :: Int -> Int
paresNenCirculo r = length [
    (x, y) |
    x <- [0..(pred r)],
    y <- [0..(pred r)],
    x^2 + y^2 < r^2]

expErr :: Float -> Int
expErr err = auxErr 1 where
    auxErr m
        | exp 1 - aprx < err = 1
        | otherwise          = succ . auxErr $ succ m
        where aprx = (1 + 1/m)**m

ternasPit :: Integral a => a -> [(a, a, a)]
ternasPit n = [(x, y, z) |
                x <- [1..n],
                y <- [1..n],
                z <- [1..n],
                x^2+y^2==z^2]

productoEscalar :: [Int] -> [Int] -> Int
productoEscalar [] _          = 0
productoEscalar _ []          = 0
productoEscalar (x:xs) (y:ys) = x*y + productoEscalar xs ys

sumaConsecutivos :: [Int] -> [Int]
sumaConsecutivos xs = zipWith (+) xs (tail xs)

posiciones :: (Eq a) => a -> [a] -> [Int]
posiciones e xs = auxPos e xs 0 where
    auxPos _ [] _ = []
    auxPos e (x:xs) state
        | e == x    = state : loop
        | otherwise = loop
        where loop = auxPos e xs (succ state)

densa :: [Int] -> [(Int, Int)]
densa pol = auxDensa pol (pred . length $ pol) where
    auxDensa [] _ = []
    auxDensa (coef:pol) exponente
        | coef == 0 = loop
        | otherwise = (exponente, coef) : loop
        where loop = auxDensa pol (pred exponente)

dispersa :: [(Int, Int)] -> [Int]
dispersa pol = auxDisp pol (succ . fst . head $ pol) where
    auxDisp [] _ = []
    auxDisp ((exponente, coef):pol) prexp
        | prexp - exponente == 1 = loop
        | otherwise              = zeros ++ loop
        where loop = coef : auxDisp pol exponente
              zeros = replicate (pred (prexp - exponente)) 0

-- Combining 'range' and 'length' it is shown
-- that naturals are isomorphic to lists
-- length :: [a] -> Int
range :: (Enum a, Num a) => Int -> [a]
range n = auxRange n 0 where
    auxRange 0 _ = []
    auxRange n i = i : auxRange (pred n) (succ i)

data Lista a = Vacia | Cons a (Lista a)

factorial :: Int -> Maybe Int
factorial n
    | n < 0 = Nothing
    | n < 2 = Just 1
    | otherwise = (*n) <$> (factorial . pred $ n)

mcd :: Int -> Int -> Int
mcd a 0 = a
mcd a b = mcd b (mod a b)

stepHanoi :: Int -> Int
stepHanoi 1 = 1
stepHanoi d = succ . (*2) . stepHanoi . pred $ d

refinada :: [Float] -> [Float]
refinada [] = []
refinada [x] = [x]
refinada (x:xs) = x : m : refinada xs
    where m = (x + head xs) / 2
