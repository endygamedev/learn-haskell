# How monads work?
In this article we will see with an example what monad is.

## Monad typeclass
In the class `Monad`, 4 operators are implemented: `return`, `(>>=)`, `(>>)` and `fail`.
```haskell
class Monad m where
  return :: a -> m a

  (>>=) :: m a -> (a -> m b) -> m b  -- bind

  x >> y = x >>= \_ -> y

  fail :: String -> m a
  fail s = error s
```
`(a -> m b)` — Kleisli arrow.

## Example
### Arrow Kleisli
Okay, let's now see an example of how bind works, because it is the main operator in the Monad class.
###### All code is [here](../src/MonadExample.hs).

Suppose we have an `arrowf` function that takes some number that is a representative of the `Enum` and `Show` class and returns this number added by 1 and wraps it in a `String`, and then puts it into the `Just` container.
```haskell
arrowf :: (Enum a, Show a) => a -> Maybe String
arrowf x = Just (show $ succ x)
```
If we look at the type of this function, it will look like this: `(Enum a, Show a) => a -> Maybe String`. Now take another look at the type of the `(>>=)` function: `m a -> (a -> m b) -> m b`. If we remove that a is a representative of the `Enum` and `Show` classes, then it turns out that we have written a specific example of a Kleisley arrow: `(a -> m b)`, where `m = Maybe` and `b = String`.

Usage example:
```haskell
MonadExample> arrowf 3
Just "4"
```
### Bind function
Okay, now let's move on to writing an example bind function.
```haskell
monadBind :: Maybe a -> (a -> Maybe String) -> Maybe String
monadBind Nothing _ = Nothing
monadBind (Just x) f = f x
```
The `monadBind` function is an exact example of the bind function when: `m = Maybe` and `b = String`.

Usage example:
```haskell
MonadExample> monadBind (Just 3) arrowf
Just "4"
```

We can also use the `return` function:
```haskell
MonadExample> monadBind (return 3) arrowf
Just "4"
```
All other operators in the Monad class are fairly self-explanatory.
