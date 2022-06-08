module Numeros where

data Numero = Cero | P Numero

instance Show Numero where
    show Cero = "0"
    show (P a) = "|" ++ show a

suma :: Numero -> Numero -> Numero
suma Cero n = n
suma n Cero = n
suma (P a) b = suma a (P b) 

multiplicacion :: Numero -> Numero -> Numero
multiplicacion Cero n = Cero
multiplicacion (P a) b = suma b (multiplicacion a b)
