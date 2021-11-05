module Main where

import Control.Monad (ap, liftM)


main = undefined


newtype Reader r a = Reader { runReader :: (r -> a) }


instance Functor (Reader r) where
  fmap = liftM


instance Applicative (Reader r) where
  pure = return
  (<*>) = ap


instance Monad (Reader r)  where
  return x = Reader $ \e -> x
  m >>= k = Reader $ \e ->
    let v = runReader m e
    in runReader (k v) e

ask :: Reader r r
ask = Reader id


-- Example
type User = String
type Password = String
type UsersTable = [(User, Password)]


pwds :: UsersTable
pwds = [("Bill", "123"), ("Ann", "qwerty"), ("John", "2sRq8P")]


firstUser :: Reader UsersTable User
firstUser = do
  e <- ask
  return $ fst (head e)

-- runReader firstUser pwds


asks :: (r -> a) -> Reader r a
asks = Reader


firstUserPwd :: Reader UsersTable Password
firstUserPwd = do
  pwd <- asks (snd . head)
  return pwd


firstUserPwd' :: Reader UsersTable Password
firstUserPwd' = asks (snd . head)

-- runReader firstUserPWd pwds


usersCount :: Reader UsersTable Int
usersCount = asks length


-- Modify enviroment
local :: (r -> r) -> Reader r a -> Reader r a
local f m = Reader $ \e -> runReader m (f e)


localTest :: Reader UsersTable (Int, Int)
localTest = do
  count1 <- usersCount
  count2 <- local (("Mike", "1"):) usersCount
  return (count1, count2)


reader :: (r -> a) -> Reader r a
reader f = do
  r <- ask
  return (f r)


usersWithBadPasswords :: Reader UsersTable [User]
usersWithBadPasswords = do
  pwd <- ask
  return $ map fst (filter (\x -> snd x == "123456") pwd)
