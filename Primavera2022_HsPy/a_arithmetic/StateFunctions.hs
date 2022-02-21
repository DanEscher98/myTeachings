module StateFunctions where

import           Control.Monad.State
import           Data.Function
import           Data.Map            as Map
import           Data.Maybe

type Memo a b = State (Map.Map a b) b

-- memoFunk :: (a -> Memo a b) -> a -> Memo a b
-- memoFunk f x = do
--     gets (Map.lookup x)
--     maybe work return where
--         work = do
--             y <- f x
--             modify $ Map.insert x y
--             return y
--
-- fibMem :: [Int] -> [Int]
-- fibMem ns = evalState (mapM fib ns)

factorial n = fix fact n

fact :: (Int -> Int) -> Int -> Int
fact f n | n < 2 = 1
         | otherwise = n * (f . pred $ n)

fib :: (Int -> Int) -> Int -> Int
fib f n | n < 2 = n
        | otherwise = f (n - 1) + f (n - 2)

collatz :: (Int -> Int) -> Int -> Int
collatz _ 1 = 0
collatz f n = succ . f . flip div 2 $ aux n where
    aux n | rem n 2 == 0 = n
          | otherwise    = succ (3 * n)
