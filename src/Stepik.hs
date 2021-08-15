module Stepik where

import Prelude
import Data.Char
import Data.Function
import Data.List (transpose)


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


{- Task -}
class Printable a where
    toString :: a -> String

instance Printable Bool where
  toString True = "true"
  toString False = "false"

instance Printable () where
  toString _ = "unit type"

instance (Printable a, Printable b) => Printable (a, b) where
  toString (x,y) = "(" ++ toString x ++ "," ++ toString y ++ ")"


{- Task -}
class KnownToGork a where
    stomp :: a -> a
    doesEnrageGork :: a -> Bool

instance KnownToGork Int where
  stomp _ = 4
  doesEnrageGork = (< 10)


class KnownToMork a where
    stab :: a -> a
    doesEnrageMork :: a -> Bool


instance KnownToMork Int where
  stab _ = 3
  doesEnrageMork = (> 5)


class (KnownToGork a, KnownToMork a) => KnownToGorkAndMork a where
    stompOrStab :: a -> a


instance KnownToGorkAndMork Int where
    stompOrStab x
        | doesEnrageGork x && doesEnrageMork x = stomp $ stab x
        | doesEnrageGork x = stab x
        | doesEnrageMork x = stomp x
        | otherwise = x


{- Task -}
class (Enum a, Bounded a, Eq a) => SafeEnum a where
  ssucc :: a -> a
  ssucc x | maxBound == x = minBound | otherwise = succ x
--  ssucc maxBound = minBound
--  ssucc x = succ x

  spred :: a -> a
  spred x | minBound == x = maxBound | otherwise = pred x
--  spred minBound = maxBound
--  spred x = pred x


instance SafeEnum Bool
instance SafeEnum Int


{- Task -}
avg :: Int -> Int -> Int -> Double
avg x y z = sum(map fromIntegral [x,y,z]) / 3


{- Task -}
--bar x y z = x + y
--foo a b = bar a a (a + b)
--value = foo (3 * 10) (5 - 2)

{-
value ~> foo (3 * 10) (5 - 2) ~> bar (3 * 10) (3 * 10) ((3* 10) + (5 - 2)) ~>
(3 * 10) + (3 * 10) ~> 30 + (3 * 10) ~> 30 + 30 ~> 60
-}


{- Task -}
addTwoElements :: a -> a -> [a] -> [a]
addTwoElements x y lst = x : y : lst


{- Task -}
nTimes:: a -> Int -> [a]
nTimes x 0 = []
nTimes x n = x : nTimes x (n-1)


{- Task -}
oddsOnly :: Integral a => [a] -> [a]
oddsOnly xs = [x | x <- xs, odd x]


oddsOnly' :: Integral a => [a] -> [a]
oddsOnly' [] = []
oddsOnly' (x:xs)
  | odd x = x : oddsOnly' xs
  | otherwise = oddsOnly' xs


reverse' :: Integral a => [a] -> [a]
reverse' l = rev l [] where
  rev [] a = a
  rev (x:xs) a = rev xs (x:a)


{- Task -}
isPalindrome :: Eq a => [a] -> Bool
isPalindrome xs = helper xs True where
  helper _ False = False
  helper [] b = b
  helper [_] b = b
  helper xs b = helper (tail $ init xs) (head xs == last xs)


{- Task -}
sum3 :: Num a => [a] -> [a] -> [a] -> [a]
sum3 as bs cs = map sum $ transpose [as, bs, cs]


{- Task -}
groupElems :: Eq a => [a] -> [[a]]
groupElems xs = let
                  helper [] res = res
                  helper (x:xs) [] = helper xs [[x]]
                  helper (x:xs) res
                    | (last $ lres) == x = helper xs (init res ++ [x:lres])
                    | otherwise = helper xs (res ++ [[x]])
                    where lres = last res
                in helper xs []