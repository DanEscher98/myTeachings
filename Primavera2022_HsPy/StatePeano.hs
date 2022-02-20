module PeanoNat where

import           Control.Monad
import           Control.Monad.State
import           Data.Maybe

-- Cero es natural
-- El sucesor de un natural
--  tambien es natural

data Nat = Zero | S Nat

instance Show Nat where
    show Zero  = "0"
    show (S n) = "S" ++ show n

add :: Nat -> Nat -> Maybe Nat
add Zero n  = Just n
add (S n) m = (Just . S) m >>= add n

mul :: Nat -> Nat -> Maybe Nat
mul Zero _  = Just Zero
mul (S n) m = (mul n m) >>= add m

sub :: Nat -> Nat -> Maybe Nat
sub n Zero      = Just n
sub Zero _      = Nothing
sub (S m) (S n) = sub m n

division :: Nat -> Nat -> Maybe (Nat, Nat) -- (rem, qot)
division _ Zero = Nothing
division a b    = Just (runState (loop a b) Zero) where
    loop a b    = do
        case (sub a b) of
          Nothing -> return a
          Just a' -> modify S >> loop a' b

liftJoin2 :: Monad m => (a -> b -> m c) -> m a -> m b -> m c
liftJoin2 f x y = join $ f <$> x <*> y

toPeano :: Int -> Maybe Nat
toPeano 0 = Just Zero
toPeano n
    | n < 0 = Nothing
    | otherwise = (toPeano . pred) n >>= (return . S)

divInt :: Int -> Int -> Maybe (Nat, Nat)
divInt a b = liftJoin2 division (toPeano a) (toPeano b)

test = do
    let n = toPeano 13
    let m = toPeano (-2)
    let q = toPeano 4
    putStrLn . show . fromJust $ division <$> n <*> q
--  putStrLn . show $ (add m n >>= mul m >>= flip sub q)
