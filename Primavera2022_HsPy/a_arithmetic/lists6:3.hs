module Listas where

cabeza :: [a] -> a
cabeza []    = error "Lista vacia"
cabeza (x:_) = x

cola :: [a] -> [a]
cola []     = error "Lista vacia"
cola (_:xs) = xs

longitud :: [a] -> Int
longitud []     = 0
longitud (_:xs) = succ (longitud xs)

toma :: Int -> [a] -> [a]
toma _ []     = []
toma 0 _      = []
toma n (x:xs) = x : (toma (pred n) xs)

tira :: Int -> [a] -> [a]
tira _ []     = []
tira 0 xs     = xs
tira n (_:xs) = tira (pred n) xs
