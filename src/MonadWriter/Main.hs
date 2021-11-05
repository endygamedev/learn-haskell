module Main where

import Data.Monoid
import Control.Monad (ap, liftM)


main = undefined


newtype Writer w a = Writer { runWriter :: (a,w) }


writer :: (a,w) -> Writer w a
writer = Writer


execWriter :: Writer w a -> w
execWriter m = snd (runWriter m)


instance (Monoid w) => Functor (Writer w) where
  fmap = liftM


instance (Monoid w) => Applicative (Writer w) where
  pure = return
  (<*>) = ap


instance (Monoid w) => Monad (Writer w) where
  return x = Writer (x,mempty)
  m >>= k =
    let (x,u) = runWriter m
        (y,v) = runWriter $ k x
    in Writer (y, u `mappend` v)


-- Examples:
-- runWriter (return 3 :: Writer String Int)
-- runWriter (return 3 :: Writer (Sum Int) Int)
-- runWriter (return 3 :: Writer (Product Int) Int)
-- execWriter (return 3 :: Writer (Product Int) Int)


evalWriter :: Writer w a -> a 
evalWriter = fst . runWriter


tell :: Monoid w => w -> Writer w ()
tell w = writer ((), w)


calc :: (Int -> Int -> Int) -> Int -> Int -> Writer String Int
calc op arg1 arg2 = do
  let res = arg1 `op` arg2
  tell "ok "
  if abs res < 128 then
    return res
  else do
    tell "overflow"
    return res


-- Examples:
-- execWriter $ calc (+) 33 44
-- execWriter $ calc (+) 99 44


type Shopping = Writer [(Integer, String)] ()


purchase :: String -> Integer -> Shopping
purchase item cost = writer ((), [(cost, item)])


total :: Shopping -> Integer
total = sum . map fst . execWriter


items :: Shopping -> [String]
items = map snd . execWriter


shopping1 :: Shopping
shopping1 = do
  purchase "Jeans"   19200
  purchase "Water"     180
  purchase "Lettuce"   328
