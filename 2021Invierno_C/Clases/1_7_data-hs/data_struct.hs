module DataStruct where

data Lista a = Vacia | Cons a (Lista a)

cabeza :: Lista a -> a
cabeza Vacia = error "Lista vacia"
cabeza (Cons x _) = x

cola :: Lista a -> Lista a
cola Vacia = error "Lista vacia"
cola (Cons _ xs) = xs

instance Show a => Show (Lista a) where
    show Vacia = "[]"
    show (Cons x xs) = show x ++ ":" ++ show xs

mapear :: (a -> b) -> [a] -> [b]
mapear _ [] = []
mapear f (x:xs) = f x : mapear f xs

suma :: [Int] -> Int
suma xs = plegari (+) 0 xs

plegari :: (b -> a -> b) -> b -> [a] -> b
plegari _ e [] = e
plegari f e (x:xs) = plegari f (f e x) xs

product :: [Int] -> Int
product xs = plegari (*) 1 xs

-- int cabeza(Lista a)
-- float cabeza_f(Lista_f a)
