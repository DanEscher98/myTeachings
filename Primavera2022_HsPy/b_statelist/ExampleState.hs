module ExampleState where

import           Control.Monad.State (State, evalState, get, mapM, modify, put,
                                      runState)
import           Data.Char
import           System.IO

noZero :: Int -> Maybe Int
noZero n =
    if n == 0
       then Nothing
       else Just n

counter :: Int -> State Int Int
counter j = do
    i <- get
    put (succ i)
    return (j * i)

factorial :: Int -> Int
factorial n = evalState (auxFac n) 1 where
    auxFac n = do
        cache <- get
        if n == 0
           then return cache
           else do
               modify (*n)
               auxFac (pred n)

fibonacci :: Int -> Int
fibonacci n = evalState (auxFib n) (0, 1) where
    auxFib 0 = return 0
    auxFib n = do
        (a, b) <- get
        if n == 1
           then return b
           else do
               put (b, a + b)
               auxFib (pred n)


main :: IO ()
main = do
    print $ evalState (counter 4) 0
    c <- getChar
    print c
