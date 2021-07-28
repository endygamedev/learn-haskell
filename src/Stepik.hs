module Stepik where

import Prelude
import Data.Char


main = undefined


{- Create own operator -}
infixl 6 *+*
(*+*) a b = a ^ 2 + b ^ 2


infixl 6 |-|
x |-| y = abs (x - y)


twoDigits2Int :: Char -> Char -> Int
twoDigits2Int x y = if isDigit x && isDigit y then read [x, y] :: Int else 100


doubleFact :: Integer -> Integer
doubleFact 1 = 1
doubleFact 2 = 2
doubleFact n = n * doubleFact(n-2)


{- Negative Factorial -}
fibonacci :: Integer -> Integer
fibonacci 0 = 0
fibonacci 1 = 1
fibonacci n 
    | n < 0 = (-1)^(abs n + 1) * fibonacci (abs n)
    | otherwise = fibonacci (n - 1) + fibonacci (n - 2)


{- Tail recursion -}
factorial :: Integer -> Integer
factorial n
    | n >= 0 = helper 1 n
    | otherwise = error "Arg must be >= 0!"


helper :: Integer -> Integer -> Integer
helper acc 0 = acc
helper acc n = helper (acc * n) (n - 1)


{- Negative Fibonacci tail recursion -}
fibonacciTail :: Integer -> Integer
fibonacciTail n
    | n < 0 = (-1)^(abs n + 1) * fibonacciTail (abs n)
    | otherwise = helperFib 0 1 n


helperFib :: Integer -> Integer -> Integer -> Integer
helperFib p c 1 = c
helperFib p c n = helperFib c (c+p) (n-1)


{- Negative Fibonacci tail recursion `let ... in ...`. Task -}
fibonacciTail' :: Integer -> Integer
fibonacciTail' n
    | n < 0 = (-1)^(abs n + 1) * fibonacciTail' (abs n)
    | otherwise = let 
        helperFib p c 0 = 0
        helperFib p c 1 = c
        helperFib p c n = helperFib c (c + p) (n - 1)
    in helperFib 0 1 n


fibonacciTail'' :: Integer -> Integer
fibonacciTail'' n
    | n < 0 = (-1)^(abs n + 1) * fibonacciTail'' (abs n)
    | otherwise = helper 0 1 n
    where
        helper p c 0 = 0
        helper p c 1 = c
        helper p c n = helper c (c + p) (n - 1)


{- Task -}
seqA :: Integer -> Integer
seqA 0 = 1
seqA 1 = 2
seqA 2 = 3
seqA n = seqA (n-1) + seqA (n-2) - 2 * seqA (n-3)


seqA' :: Integer -> Integer
seqA' n = helper 3 3 2 n 
    where helper curr prev multip n
            | n == 0 = 1
            | n == 1 = 2
            | n == 2 = 3
            | n == 3 = curr
            | n > 3 = helper (curr + prev - 2 * multip) curr prev (n - 1)


seqA'' :: Integer -> Integer
seqA'' n = let
    helper curr prev multip 0 = 1
    helper curr prev multip 1 = 2
    helper curr prev multip 2 = 3
    helper curr prev multip 3 = curr
    helper curr prev multip n = helper (curr + prev - 2 * multip) curr prev (n - 1)
    in helper 3 3 2 n


seqA''' :: Integer -> Integer
seqA''' n = helper 3 3 2 n 
    where
        helper curr prev multip 0 = 1
        helper curr prev multip 1 = 2
        helper curr prev multip 2 = 3
        helper curr prev multip 3 = curr
        helper curr prev multip n = helper (curr + prev - 2 * multip) curr prev (n - 1)


{- Task -}
sum'n'count :: Integer -> (Integer, Integer)
sum'n'count x = (s, l)
        where
            xs = show $ abs x
            l = toInteger $ length xs
            s = toInteger $ sum $ map digitToInt xs
