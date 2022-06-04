module Main where

import           Data.List
import           ErrorEncapsulated (getValue)
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
    hPutStr tmpHandle newTodoItems -- Write newItems in tmp
    hClose tmpHandle -- Close tmp file
    removeFile "todo.txt"
    renameFile tmpName "todo.txt"
    hPutStr stdout "Goodbye!"
