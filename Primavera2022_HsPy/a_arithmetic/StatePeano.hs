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

divNat :: Nat -> Nat -> Maybe (Nat, Nat) -- (rem, qot)
divNat _ Zero = Nothing
divNat a b    = Just (runState (loop a b) Zero) where
    loop a b  = do
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
divInt a b = liftJoin2 divNat (toPeano a) (toPeano b)

equal :: Nat -> Nat -> Bool
equal Zero Zero   = True
equal Zero _      = False
equal _ Zero      = False
equal (S a) (S b) = equal a b

intProp1 :: Int -> [(Int, (Int, Int))]
intProp1 n = do
    x <- [1..n]
    y <- [1..n]
    let r = rem x y
    guard (r == quot x y)
    return (r, (x, y))

intProp2 n =
    takeWhile (not . null)
    . concat
    . map (takeWhile  ((<20) . fst)
        . (\n -> [(n * (b + 1), b) | b <- [1..]]))
    $ [1..n]


test = do
    let n = toPeano 13
    let m = toPeano (-2)
    let q = toPeano 4
    putStrLn . show . fromJust $ divNat <$> n <*> q
--  putStrLn . show $ (add m n >>= mul m >>= flip sub q)
