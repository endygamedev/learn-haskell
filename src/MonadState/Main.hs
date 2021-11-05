module Main where

import Control.Monad (ap, liftM, replicateM)


main = undefined


newtype State s a = State { runState :: s -> (a,s) }


-- runState :: State s a -> s -> (a,s)

instance Functor (State s) where
  fmap = liftM


instance Applicative (State s) where
  pure = return
  (<*>) = ap


instance Monad (State s) where
  return a = State $ \st -> (a,st)
  m >>= k = State $ \st ->
    let (a,st') = runState m st
        m' = k a
    in runState m' st'


execState :: State s a -> s -> s
execState m s = snd (runState m s)


evalState :: State s a -> s -> a
evalState m s = fst (runState m s)


get :: State s s
get = State $ \st -> (st, st)

-- runState get 5


put :: s -> State s ()
put st = State $ \_ -> ((), st)

-- runState (put 7) 5


tick :: State Int Int
tick = do
  n <- get
  put (n+1)
  return n

-- runState tick 5


modify :: (s -> s) -> State s ()
modify f = State $ \s -> ((), f s)


modify' :: (s -> s) -> State s ()
modify' f = do
  s <- get
  put (f s)

-- runState (modify (*3)) 5


succ' :: Int -> Int
succ' n = execState tick n

-- succ' 3


plus :: Int -> Int -> Int
plus n x = execState (sequence $ replicate n tick) x

-- 4 `plus` 5


{-
replicateM :: (Monad m) => Int -> m a -> m [a]
replicateM n = sequence . replicate n
-}


plus' :: Int -> Int -> Int
plus' n x = execState (replicateM n tick) x

-- 4 `plus'` 5
-- 4 `plus` 5


fibStep :: State (Integer, Integer) ()
fibStep = do
  (a, b) <- get
  put (b,a+b)


execStateN :: Int -> State s a -> s -> s
execStateN n m = execState $ replicateM n m


fib :: Int -> Integer
fib n = fst $ execStateN n fibStep (0, 1)


data Tree a = Leaf a | Fork (Tree a) a (Tree a)


numbering :: Tree () -> State Integer (Tree Integer)
numbering (Leaf _) = do
	n <- get
	put (n+1)
	return (Leaf n)
numbering (Fork l _ r) = do
	left <- numbering l
	n <- get
	put (n+1)
	right <- numbering r
	return (Fork left n right)


numberTree :: Tree () -> Tree Integer
numberTree tree = evalState (numbering tree) 1
