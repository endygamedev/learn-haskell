module HW1 where

import Prelude(Bool(..), Eq(..), Show(..), (||), (-), (*), Int)
import Nat


beside :: Nat -> Nat -> Bool
beside a b = (a == Succ b) || (Succ a == b)

beside2 :: Nat -> Nat -> Bool
beside2 a b = (a == Succ (Succ b)) || (Succ (Succ a) == b)

pow :: Int -> Int -> Int
pow x 1 = x
pow x n = x * pow x (n-1)
