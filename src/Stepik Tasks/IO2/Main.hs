module Main where

import Control.Monad
import System.Directory
import System.IO
import Data.List

printOut :: [[Char]] -> IO ()
printOut [] = putStrLn "Canceled\n"
printOut x = mapM_ (\y -> putStrLn $ "Removing file: " ++ y) x 


removeFiles :: [[Char]] -> IO ()
removeFiles = mapM_ removeFile


main' :: IO ()
main' = do
  putStr "Substring: "
  hFlush stdout
  str <- getLine
  files <- getDirectoryContents "."
  if null str then
    putStrLn "Canceled"
  else do
    let filtered = filter (\x -> isInfixOf str x && x /= "." && x /= "..") files
    printOut filtered
    removeFiles filtered

main :: IO ()
main = main'
