module Example where

funk1 :: [Int] -> [Int]
funk1 xs = xs >>= (\x -> [x * x]) >>= (\y -> replicate 3 y)

funk1' :: [Int] -> [Int]
funk1' xs = do
    x <- xs
    y <- [x*x]
    replicate 3 y

    {-
        int funk(int n)
            x = n
            y = x*x
            return x
    -}

data Quizas a = Nada | Un a

instance Show a => Show (Quizas a) where
    show Nada   = ":'v"
    show (Un x) = ":v/ " ++ show x

instance Functor Quizas where
    fmap _ Nada   = Nada
    fmap f (Un x) = Un (f x)

instance Applicative Quizas where
    pure x = Un x
    Nada <*> _        = Nada
    _ <*> Nada        = Nada
    (Un f) <*> (Un x) = Un (f x)

instance Monad Quizas where
    (>>=) :: Quizas a -> (a -> Quizas b) -> Quizas b
    Nada >>= _   = Nada
    (Un x) >>= f = f x

-- Monada de Lista
data Lista a = Null | Cons a (Lista a)

plegarD :: (a -> b -> b) -> b -> (Lista a) -> b
plegarD _ e Null        = e
plegarD f e (Cons x xs) = f x (plegarD f e xs)

plegarI :: (b -> a -> b) -> b -> (Lista a) -> b
plegarI f e Null        = e
plegarI f e (Cons x xs) = plegarI f (f e x) xs

concatena :: Lista a -> Lista a -> Lista a
concatena xs ys = plegarD (Cons) xs ys

invertir :: Lista a -> Lista a
invertir xs = plegarI (flip (Cons)) Null xs

instance Show a => Show (Lista a) where
    show Null        = "[]"
    show (Cons x xs) = show x ++ ":" ++ show xs

instance Functor Lista where
    fmap _ Null        = Null
    fmap f (Cons x xs) = Cons (f x) (fmap f xs)

instance Applicative Lista where
    pure x = Cons x Null
    Null <*> _ = Null
    _ <*> Null = Null
    (Cons f fs) <*> xs =
        concatena (fmap f xs) (fs <*> xs)

instance Monad Lista where
    Null >>= _        = Null
    (Cons x xs) >>= f = concatena (f x) (xs >>= f)
