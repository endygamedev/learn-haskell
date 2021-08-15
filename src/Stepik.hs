module Stepik where

import Prelude
import Data.Char
import Data.Function


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


{- Task -}
integration :: (Double -> Double) -> Double -> Double -> Double
integration f a b = let
                        xs = [a + j * h | j <- [0..1000]]
                        h = (b - a) / 1000
                    in sum [( ( f (xs !! i) + f (xs !! (i + 1)) ) / 2 ) * ( xs !! (i + 1) - xs !! i ) | i <- [0..999]]


integration' :: (Double -> Double) -> Double -> Double -> Double
integration' f a b = h * ( (f a + f b) / 2 + (sum $ map f xs) )
    where
        n = 1000
        h = (b - a) / n
        xs = [a + h * x | x <- [0..n-1]]


{- Task -}
getSecondFrom :: t1 -> t2 -> t3 -> t2
getSecondFrom x y z = y


{- Task -}
multSecond = g `on` h

g :: (Num a) => a -> a -> a
g x y = x * y

h :: (a, b) -> b
h (x, y) = y


{- Task -}
on3 :: (b -> b -> b -> c) -> (a -> b) -> a -> a -> a -> c
on3 op f x y z = op (f x) (f y) (f z)


{- Task -}
doItYourself = doItYourselfF . doItYourselfG . doItYourselfH

doItYourselfF = logBase 2
doItYourselfG = (^ 3)
doItYourselfH = max 42


{- Task -}
swap' = uncurry (flip (,))