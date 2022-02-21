module PeanoNat where

import           Control.Monad

-- Cero es natural
-- El sucesor de un natural
--  tambien es natural

data Nat = Zero | S Nat

instance Show Nat where
    show Zero  = "0"
    show (S n) = "S" ++ show n

data Quizas a = Nada | Un a

instance Show a => Show (Quizas a) where
    show Nada   = "_/(._.)"
    show (Un a) = "\\(^_^) " ++ show a

instance Functor Quizas where
    fmap _ Nada   = Nada
    fmap f (Un a) = Un (f a)

instance Applicative Quizas where
    pure x = Un x
    Nada <*> _        = Nada
    _ <*> Nada        = Nada
    (Un f) <*> (Un x) = Un (f x)

instance Monad Quizas where
    Nada >>= _   = Nada
    (Un x) >>= f = f x

add :: Nat -> Nat -> Quizas Nat
add Zero n  = Un n
add (S n) m = add n (S m)
-- add (S n) = add n . S

mul :: Nat -> Nat -> Quizas Nat
mul Zero _  = Un Zero
mul (S n) m = mul n m >>= add n
            -- add m (mul n m)

sub :: Nat -> Nat -> Quizas Nat
sub Zero _      = Nada
sub n Zero      = Un n
sub (S m) (S n) = sub m n

minor :: Nat -> Nat -> Bool
minor Zero Zero   = False
minor _ Zero      = False
minor Zero _      = True
minor (S m) (S n) = minor m n

qot :: Nat -> Nat -> Quizas Nat
qot _ Zero = Nada
qot m n = case (sub m n) of
            Nada    -> Un Zero
            (Un m') -> fmap S (qot m' n)

res :: Nat -> Nat -> Quizas Nat
res m n = case (sub m n) of
            Nada    -> Un m
            (Un m') -> res m' n

-- divNat :: Nat -> Nat -> Quizas (Nat, Nat)

-- res :: Nat -> Nat -> Nat
-- res m n | minor m n = m
--         | otherwise = res (sub m n) n
--
-- qot :: Nat -> Nat -> Nat
-- qot m n | minor m n = Zero
--         | otherwise = (S . qot (sub m n)) n

toPeano :: Int -> Nat
toPeano 0 = Zero
toPeano n | n < 0 = error "numero negativo"
          | otherwise = (S . toPeano . pred) n
