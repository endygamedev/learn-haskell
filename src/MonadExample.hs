module MonadExample where

import Prelude


arrowf :: (Enum a, Show a) => a -> Maybe String
arrowf x = Just (show $ succ x)


monadBind :: Maybe a -> (a -> Maybe String) -> Maybe String
monadBind Nothing _ = Nothing
monadBind (Just x) f = f x
