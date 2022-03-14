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
    pure x = Cons x Null
    Null 

