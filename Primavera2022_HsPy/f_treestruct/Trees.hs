module EstructuraArborea where

import           Data.List (partition)

hanoi :: Int -> Int
hanoi 1 = 1
hanoi n = 2 * (hanoi (pred n)) + 1

quicksort :: (Ord a) => [a] -> [a]
quicksort []  = []
quicksort (x:xs) = menores ++ [x] ++ mayores where
    (menores, mayores) = both quicksort . partition (<x) $ xs
    both f (x, x') = (f x, f x')
