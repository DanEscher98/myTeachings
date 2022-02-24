module Main where

import           Control.Exception
import           Control.Monad
import           Data.Char
import           Data.List
import           Data.Maybe
import           System.Directory
import           System.IO


main :: IO ()
main = do
    contents <- readFile "todo.txt"
    let todoTasks = lines contents
        numberedTasks = zipWith (\n line ->
            show n ++ ") " ++ line) [0..] todoTasks
    putStrLn "These are your TO-DO items:"
    mapM_ putStrLn numberedTasks
    n <- getValue "Which one do you want to delete?: "
    let newTodoItems = unlines $ delete (todoTasks !! n) todoTasks
    (tmpName, tmpHandle) <- openTempFile "." "temp"
    hPutStr tmpHandle newTodoItems
    hClose tmpHandle
    removeFile "todo.txt"
    renameFile tmpName "todo.txt"

prompt :: String -> IO String
prompt text = putStr text >> hFlush stdout >> getLine

maybeRead :: Read a => String -> Maybe a
maybeRead s = let s' = takeWhile (not . isSpace) s
               in case reads s' of
                    [(x, "")] -> Just x
                    _         -> Nothing

myerr = error "try"

getValue :: Read a => String -> IO a
getValue msg = do
    input <- (return . maybeRead <=< prompt) msg
    case input of
      Nothing -> getValue "Try again: "
      Just n  -> return n

testExc :: forall a . a -> IO (Maybe a)
testExc expr = do
    result <- (try . evaluate) expr :: IO (Either SomeException a)
    case result of
        Left _  -> return Nothing
        Right x -> return $ Just x

-- https://stackoverflow.com/questions/5121371/how-to-catch-a-no-parse-exception-from-the-read-function-in-haskell
