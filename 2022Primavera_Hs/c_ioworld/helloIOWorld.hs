module Main (main) where
import           Data.Char
import           System.Directory
import           System.IO

main :: IO ()
main = do
    name <- prompt "Set your name: "
    let greeting = "Hello " ++ name ++ "!"
    putStrLn greeting
--    ifile <- getValidFile "Input file: "
--    ofile <- getValidFile "Output file: "
--    s <- readFile ifile
--    writeFile ofile (map toLower (filter (not . isNumber) s))
    putStrLn "Goodbye!"

getValidFile :: String -> IO String
getValidFile text = do
    file <- prompt text
    exists <- doesFileExist file
    if exists then return file
              else getValidFile "Try again: "

prompt :: String -> IO String
prompt text = putStr text >> hFlush stdout >> getLine
