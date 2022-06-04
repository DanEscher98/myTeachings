module StateFunctions where

import           Control.Monad       ((<=<))
import           Control.Monad.State (State, evalState, get, mapM, mapM_,
                                      modify, put, runState)
import           Data.Char
import           Data.Function
import qualified Data.Map            as Map (Map, adjust, empty, fromList,
                                             insert, lookup, member, toList)
import           Data.Maybe
import           System.IO

type MemoST a b = State (Map.Map a b) b

factSHT :: [Int] -> [Int]
factSHT ns = evalState (mapM loop ns) baseCases where
    baseCases = (Map.fromList [(0, 1), (1, 1)])
    loop :: Int -> MemoST Int Int
    loop k = do
        memo <- get
        case Map.lookup k memo of
          Just v -> return v
          Nothing -> do
              v' <- loop (pred k)
              let v = k * v'
              modify (Map.insert k v)
              return v

fibSHT :: [Int] -> [Int]
fibSHT ns = evalState (mapM loop ns) baseCases where
    baseCases = Map.fromList [(0, 0), (1, 1)]
    loop k = do
        memo <- get
        case Map.lookup k memo of
          Just v -> return v
          Nothing -> do
              v' <- loop (pred k)
              v'' <- loop (pred . pred $ k)
              let v = v' + v''
              modify (Map.insert k v)
              return v

collatzSHT :: [Int] -> [Int]
collatzSHT ns = evalState (mapM loop ns) baseCases where
    baseCases = Map.fromList [(1, 0)]
    loop k = do
        memo <- get
        case Map.lookup k memo of
          Just v -> return v
          Nothing -> do
              let k' = if rem k 2 == 0
                          then div k 2
                          else succ (k * 3)
              v' <- loop k'
              let v = succ v'
              modify (Map.insert k v)
              return v

hanoiSHT :: [Int] -> [Int]
hanoiSHT ns = evalState (mapM loop ns) baseCases where
    baseCases = Map.fromList [(1, 1)]
    loop k = do
        memo <- get
        case Map.lookup k memo of
          Just v -> return v
          Nothing -> do
              v' <- (loop . pred) k
              let v = succ (2 * v')
              modify (Map.insert k v)
              return v

type FuncMemoST a b = a -> (a -> MemoST a b)

-- memoization :: (Ord a) => FuncMemoST a b -> [a] -> Map.Map a b -> [b]
-- memoization f xs = evalState (mapM (loop f) xs) where
--     loop :: Ord a => FuncMemoST a b -> a -> MemoST a b
--     loop f x = do
--         maybeVal <- get (Map.lookup x)
--         case maybeVal of
--           Just y -> return y
--           Nothing -> do
--               y <- f x (loop f) -- :: a -> (a -> MemoST a b)
--               modify (Map.insert x y)
--               return y
--
-- fact :: FuncMemoST Int Int
-- fact n loop = do
--     n' <- loop fact (pred n)
--     return $ n * n'

{-
    1. function (a -> b) -> a -> b
    2. HashTable (Map a b)
    3. State (State (Map a b) b)
    *  evalState (mapM function xs) (fromList [(0, 1), (1, 1)])

    If the value is
    -}

-- memoFunk :: (a -> Memo a b) -> a -> Memo a b
-- memoFunk f x = do
--     gets (Map.lookup x)
--     maybe work return where
--         work = do
--             y <- f x
--             modify $ Map.insert x y
--             return y


codifyNum1 :: Int -> String
codifyNum1 n = map (chr . (+97) . read . pure) (show n)

decodeNum1 :: String -> Int
decodeNum1 = read . concat . map (\c -> show $ (ord c) - 97)

codifyStr1 :: String -> String
codifyStr1 s = (concat . map auxF . Map.toList . countLetters) s where
    auxF (c,n) = if n == 1 then [c] else (show n) ++ [c]

countLetters :: String -> Map.Map Char Int
countLetters = countElem . map toLower . filter isAlpha

countElem :: (Ord a) => [a] -> Map.Map a Int
countElem xs = snd $ runState (mapM_ loop xs) Map.empty where
    loop x = do
        count <- get
        if Map.member x count
           then modify (Map.adjust succ x)
           else modify (Map.insert x 1)

fib :: (Int -> Int) -> Int -> Int
fib f n | n < 2 = n
        | otherwise = f (n - 1) + f (n - 2)

fibZ :: [Int]
fibZ = 0:1 : zipWith (+) fibZ (tail fibZ)

collatz :: (Int -> Int) -> Int -> Int
collatz _ 1 = 0
collatz f n = succ . f . flip div 2 $ aux n where
    aux n | rem n 2 == 0 = n
          | otherwise    = succ (3 * n)

hanoi :: (Int -> Int) -> Int -> Int
hanoi _ 1 = 1
hanoi f n = succ . (*2) . f . pred $ n -- 63
