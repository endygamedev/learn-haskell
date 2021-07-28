module Main where

import Prelude


main = undefined


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
  | otherwise                   = "You're a whale, congratulations!"


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
                               "You're a whale, congratulations!"


sayMeGuards :: (Integral a) => a -> String
sayMeGuards x
  | x == 0    = "Zero!"
  | x == 1    = "One!"
  | x == 2    = "Two!"
  | x == 3    = "Three!"
  | x == 4    = "Four!"
  | x == 5    = "Five!"
  | otherwise = "Not between 1 and 5!"


max' :: (Ord a) => a -> a -> a
max' a b
  | a > b     = a
  | otherwise = b

-- Alternative variant but less readable
-- max' a b | a > b = a | otherwise = b


compareGuards :: (Ord a) => a -> a -> Ordering
a `compareGuards` b
  | a > b     = GT
  | a == b    = EQ
  | otherwise = LT


-- Where ?!

bmiTellWhere :: (RealFloat a) => a -> a -> String
bmiTellWhere weight height
  | bmi <= skinny = "You're underweight, you emo, you!"
  | bmi <= normal = "You're supposedly normal. Pffft, I bet you're ugly!"
  | bmi <= fat    = "You're fat! Lose some weight, fatty!"
  | otherwise     = "You're a whale, congratulations!"
  where bmi = weight / height ^ 2
        skinny = 18.5
        normal = 25.0
        fat = 30.0


bmiTellWherePatterns :: (RealFloat a) => a -> a -> String
bmiTellWherePatterns weight height
  | bmi <= skinny = "You're underweight, you emo, you!"
  | bmi <= normal = "You're supposedly normal. Pffft, I bet you're ugly!"
  | bmi <= fat    = "You're fat! Lose some weight, fatty!"
  | otherwise     = "You're a whale, congratulations!"
  where bmi = weight / height ^ 2
        (skinny, normal, fat) = (18.5, 25.0, 30.0)


initials :: String -> String -> String
initials firstname lastname = [f] ++ "." ++ [l] ++ "."
    where (f:_) = firstname
          (l:_) = lastname


calcBmis :: (RealFloat a) => [(a, a)] -> [a]
calcBmis xs = [bmi w h | (w, h) <- xs]
          where bmi weight height = weight / height ^ 2


-- Написать функцию f от двух аргументов: список списков list и элемент el.
-- Функция возвращает список подсписков из list, которые содержат el.
task1 :: (Integral a) => [[a]] -> a -> [[a]]
task1 xs el = [ys | ys <- xs, el `elem` ys]


-- Let it be

cylinder :: (RealFloat a) => a -> a -> a
cylinder r h =
      let sideArea = 2 * pi * r * h
          topArea = pi * r ^ 2
      in sideArea + 2 * topArea

-- Examples:
-- 4 * (let a = 11 in a - 1) + 1
-- let square x = x * x in [square 5, square 3, square 2]
-- let (a,b,c) = (10,20,30) in a*b*c + 50
-- let [a,b,c] = [1..3] in a*b*c + 40


calcBmisLet :: (RealFloat a) => [(a, a)] -> [a]
calcBmisLet xs = [bmi | (w,h) <- xs, let bmi = w / h ^ 2]


calcBmisFat :: (RealFloat a) => [(a, a)] -> [a]
calcBmisFat xs = [bmi | (w,h) <- xs, let bmi = w / h ^ 2, bmi >= 25.0]


-- Case expressions

headCase :: [a] -> a
headCase xs = case xs of [] -> error "No head for empty list!"
                         (x:xs) -> x


describeList :: [a] -> String
describeList xs = "The list is " ++ case xs of [] -> "empty."
                                               [x] -> "a singleton list."
                                               xs -> "a longer list."


describeList' :: [a] -> String
describeList' xs = "The list is " ++ what xs
                    where what [] = "empty."
                          what [x] = "a singleton list."
                          what xs = "a longer list."