module PeanoNatural where

import           Control.Monad

-- Cero es natural
-- El sucesor de un natural
--  tambien es natural

data Natural = Zero | S Natural

instance Show Natural where
    show Zero  = "0"
    show (S n) = "S" ++ show n

add :: Natural -> Natural -> Natural
add Zero n  = n
add (S n) m = add n (S m)
-- add (S n) = add n . S

mul :: Natural -> Natural -> Natural
mul Zero _  = Zero
mul (S n) m = (add m . mul n) m
            -- add m (mul n m)

sub :: Natural -> Natural -> Natural
sub Zero _      = error "negativo"
sub n Zero      = n
sub (S m) (S n) = sub m n

minor :: Natural -> Natural -> Bool
minor Zero Zero   = False
minor _ Zero      = False
minor Zero _      = True
minor (S m) (S n) = minor m n

res :: Natural -> Natural -> Natural
res m n | minor m n = m
        | otherwise = res (sub m n) n

qot :: Natural -> Natural -> Natural
qot m n | minor m n = Zero
        | otherwise = (S . qot (sub m n)) n

toPeano :: Int -> Natural
toPeano 0 = Zero
toPeano n | n < 0 = error "numero negativo"
          | otherwise = (S . toPeano . pred) n
