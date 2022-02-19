module PeanoNatural where

import           Control.Monad

-- Cero es natural
-- El sucesor de un natural
--  tambien es natural

data Natural = Zero | Succ Natural

instance Show Natural where
    show Zero     = "0"
    show (Succ n) = "S" ++ show n

data Quizas a = Nada | Un a

instance Show a => Show (Quizas a) where
    show Nada   = "Null"
    show (Un x) = "Un(" ++ show x ++ ")"

instance Functor Quizas where
    fmap _ Nada   = Nada
    fmap f (Un x) = Un (f x)

instance Applicative Quizas where
    pure x = Un x
    Nada <*> _        = Nada
    _ <*> Nada        = Nada
    (Un f) <*> (Un x) = Un (f x)

instance Monad Quizas where
    -- join = (flip (>>=)) id
    Nada >>= _   = Nada
    (Un x) >>= f = f x

liftJoin2 :: Monad m => (a -> b -> m c) -> m a -> m b -> m c
liftJoin2 f x y = join $ f <$> x <*> y

toPeano :: Int -> Quizas Natural
toPeano 0 = Un Zero
toPeano n
    | n < 0 = Nada
    | otherwise = (toPeano . pred) n >>= (return . Succ)

add :: Natural -> Natural -> Quizas Natural
add Zero n     = Un n
add (Succ n) m = (Un . Succ) m >>= add n

mul :: Natural -> Natural -> Quizas Natural
mul Zero _     = Un Zero
mul (Succ n) m = (mul n m) >>= add m

sub :: Natural -> Natural -> Quizas Natural
sub n Zero            = Un n
sub Zero _            = Nada
sub (Succ m) (Succ n) = sub m n

test = do
    let n = (Succ (Succ (Succ (Succ (Succ Zero)))))
    let m = (Succ (Succ Zero))
    let q = (Succ Zero)
    putStrLn . show $ (add m n >>= mul m >>= flip sub q)
