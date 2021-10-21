module MonadMaybe where

import Prelude hiding (Maybe, Just, Nothing)
import Control.Monad (liftM, ap)
import qualified Control.Monad.Fail as Fail


data Maybe a = Nothing | Just a
  deriving (Eq, Ord, Show)


instance Functor Maybe where
  fmap = liftM


instance Applicative Maybe where
  pure = return
  (<*>) = ap


instance Monad Maybe where
  return x = Just x   -- return = Just

  (Just x) >>= k = k x
  Nothing >>= _ = Nothing

  (Just _) >> m = m
  Nothing >> _ = Nothing


instance Fail.MonadFail Maybe where
  fail _ = Nothing
