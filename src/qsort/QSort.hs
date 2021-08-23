module Main where

import Prelude


qsort :: Ord a => [a] -> [a]
qsort [] = []
qsort (x:[]) = [x]
qsort (x:xs) = qsort less ++ [x] ++ qsort greater
    where
        greater = [g | g <- xs,  g >= x]
        less = [l | l <- xs, l < x]


main = putStrLn . show $ qsort [10000,9999..1] 
