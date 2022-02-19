module PeanoNatural where


-- Cero es natural
-- El sucesor de un natural
--  tambien es natural

data Natural = Cero | Succ (Natural) deriving (Show)

-- Succ (Succ (Succ Cero))

suma :: Natural -> Natural -> Natural
suma Cero n     = n
suma n Cero     = n
suma (Succ n) m = suma n (Succ m)

mult :: Natural -> Natural -> Natural
mult Cero _     = Cero
mult _ Cero     = Cero
mult (Succ n) m = suma m (mult n m)

pred :: Natural -> Natural
pred Cero     = error "Num negativo"
pred (Succ n) = n

rest :: Natural -> Natural -> Natural
rest n Cero            = n
rest Cero n            = error "Numero negativo"
rest (Succ n) (Succ m) = rest n m

data Lista a = Empty | Cons a (Lista a)

cabeza :: Lista a -> Quizas a
cabeza Empty       = Nada
cabeza (Cons x xs) = Un x

data Quizas a = Nada | Un a deriving Show
