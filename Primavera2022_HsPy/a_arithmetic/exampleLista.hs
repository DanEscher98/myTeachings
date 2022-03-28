module Lista where

data Lista a = Null | Cons a (Lista a)

instance (Show a) => Show (Lista a) where
    show Null        = "[]"
    show (Cons x xs) = show x ++ ":" ++ show xs

instance Functor Lista where
    fmap :: (a -> b) -> Lista a -> Lista b
    fmap _ Null        = Null
    fmap f (Cons x xs) = Cons (f x) (fmap f xs)

instance Applicative Lista where
    pure x     = Cons x Null
    Null <*> _ = Null
    _ <*> Null = Null
    fs <*> xs  = concatena (fmap (\f -> fmap f xs) fs)

-- concat
-- foldr
-- foldl

plegarD :: (a -> b -> b) -> b -> Lista a -> b
plegarD _ e Null        = e
plegarD f e (Cons x xs) = f x (plegarD f e xs)

plegarI :: (b -> a -> b) -> b -> Lista a -> b
plegarI _ e Null        = e
plegarI f e (Cons x xs) = plegarI f (f e x) xs

concatena :: Lista (Lista a) -> Lista a
concatena Null        = Null
concatena (Cons l ls) = plegarD Cons (concatena ls) l


data Arbol a b = Fruta a | Rama b (Arbol a b) (Arbol a b)
