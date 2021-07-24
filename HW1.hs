module HW1 where

import Prelude
import Nat


beside :: Nat -> Nat -> Bool
beside a b = (a == Succ b) || (Succ a == b)


beside2 :: Nat -> Nat -> Bool
beside2 a b = (a == Succ (Succ b)) || (Succ (Succ a) == b)


-- Typeclasses and Types

pow :: (Integral a) => a -> a -> a
pow x 1 = x
pow x n = x * pow x (n-1)


removeNonUpperCase :: [Char] -> [Char]
removeNonUpperCase xs = [c | c <- xs, c `elem` ['A'..'Z']]

-- upper = ['A'..'Z']
-- lower = ['a'..'z']
-- xs = [if even x then lower !! x else upper !! x | x <- [0..(length lower)-1]]


-- Pattern Matching

sayMePattern :: (Integral a) => a -> String
sayMePattern 0 = "Zero!"
sayMePattern 1 = "One!"
sayMePattern 2 = "Two!"
sayMePattern 3 = "Three!"
sayMePattern 4 = "Four!"
sayMePattern 5 = "Five!"
sayMePattern _ = "Not between 1 and 5!"


sayMeIf :: (Integral a) => a -> String
sayMeIf x = if x == 0
            then "Zero!"
            else
              if x == 1
              then "One!"
              else
                if x == 2
                then "Two!"
                else
                  if x == 3
                  then "Three!"
                  else
                    if x == 4
                    then "Four!"
                    else
                      if x == 5
                      then "Five!"
                      else
                        "Not between 1 and 5!"


factorial :: (Integral a) => a -> a
factorial x = product [1..x]


factorialRecursive :: (Integral a) => a -> a
factorialRecursive 0 = 1
factorialRecursive x = x * factorialRecursive (x - 1)


head' :: [a] -> a
head' [] = error "Can't call `head'` for empty list, dummy!"
-- head' (x:xs) = x
head' (x:_) = x


length' :: (Num a) => [a] -> Int
length' [] = 0
length' (_:xs) = 1 + length' xs


sum' :: (Num a) => [a] -> a
sum' [] = 0
sum' (x:xs) = x + sum' xs


capital :: String -> String
capital "" = "Empty string, dummy!"
capital all@(x:_) = "The first char of string " ++ all ++ " is " ++ [x]

-- Guards
bmiTell :: (RealFloat a) => a -> a -> String
bmiTell weight height
  | weight / height ^ 2 <= 18.5 = "You're underweight, you emo, you!"
  | weight / height ^ 2 <= 25.0 = "You're supposedly normal. Pffft, I bet you're ugly!"
  | weight / height ^ 2 <= 30.0 = "You're fat! Lose some weight, fatty!"
  | otherwise                   = "You're a whale, congratualations!"


bmiTell' :: (RealFloat a) => a -> a -> String
bmiTell' weight height = if weight / height ^ 2 <= 18.5
                          then "You're underweight, you emo, you!"
                          else
                            if weight / height ^ 2 <= 25.0
                            then "You're supposedly normal. Pffft, I bet you're ugly!"
                            else
                              if weight / height ^ 2 <= 30.0
                              then "You're fat! Lose some weight, fatty!"
                              else
                               "You're a whale, congratualations!"


sayMeGuards :: (Integral a) => a -> String
sayMeGuards x
  | x == 0    = "Zero!"
  | x == 1    = "One!"
  | x == 2    = "Two!"
  | x == 3    = "Three!"
  | x == 4    = "Four!"
  | x == 5    = "Five!"
  | otherwise = "Not between 1 and 5!"