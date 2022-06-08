module NumberTheory where

data Lista a = Vacio | Cons a (Lista a)

data Natural = Cero | S Natural
instance (Show Natural) where
    show Cero = "0"
    show (S n) = "S(" ++ show n ++ ")"

data Quizas a = Nada | Un a
instance Show a => Show (Quizas a) where
    show Nada = "¯\\_(ツ)_/¯"
    show (Un x) = "(✿ ◠‿◠)-☆ " ++ show x

add :: Natural -> Natural -> Natural
add Cero b = b
add a Cero = a
add a (S b) = S (add a b)

mul :: Natural -> Natural -> Natural
mul Cero _ = Cero
mul _ Cero = Cero
mul a (S b) = add a (mul a b)

dec :: Natural -> Quizas Natural
dec Cero = Nada
dec (S x) = Un x

sub :: Natural -> Natural -> Maybe Natural
sub Cero Cero = Just Cero
sub Cero _ = Nothing
sub a Cero = Just a
sub (S a) (S b) = sub a b

igual :: Natural -> Natural -> Bool
igual Cero Cero = True
igual Cero _ = False
igual _ Cero = False
igual (S a) (S b) = igual a b

menor :: Natural -> Natural -> Bool
menor Cero Cero = False
menor Cero _ = True
menor _ Cero = False
menor (S a) (S b) = menor a b

(>>>=) :: Quizas a -> (a -> Quizas b) -> Quizas b
Nada >>>= _ = Nada
(Un x) >>>= f = f x

qot :: Natural -> Natural -> Maybe Natural
qot Cero _ = Just Cero
qot _ Cero = Nothing
qot a b | igual a b = Just (S Cero)
        | menor a b = Just Cero
        | otherwise = do
            x <- sub a b
            y <- qot x b
            Just (S y)


revertir :: [a] -> [a]
revertir xs = auxRev (xs, []) where
    auxRev ([], ys) = ys
    auxRev (x:xs, ys) = auxRev (xs, x:ys)

-- x = sub(a, b)
-- y = qot(x, b)
-- return S(y)
--        | otherwise = (sub a b) >>= (\x -> qot x b) >>= (\y -> Just (S y))

-- scanf("%d", &n);
-- mov rax 1
-- mov rdi STDIN
-- mov rsi 0x1234
-- mov rdx 4
