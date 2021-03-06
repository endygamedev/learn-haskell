module Stepik where

import Prelude hiding (Monoid, mempty, mappend, mconcat, Functor, fmap)
import Data.Char
import Data.Function
import Data.List (transpose)
import Data.Functor
-- import Data.Semigroup ((<>))
import Control.Monad (ap, liftM)

import Data.Time.Clock
import Data.Time.Format
-- import System.Locale

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


filter' :: (a -> Bool) -> [a] -> [a]
filter' p [] = []
filter' p (x:xs)
  | p x = x : filter' p xs
  | otherwise = filter' p xs


{- Task -}
readDigits :: String -> (String, String)
readDigits x = span isDigit x


{- Task -}
filterDisj :: (a -> Bool) -> (a -> Bool) -> [a] -> [a]
filterDisj p1 p2 xs = filter (\x -> p1 x || p2 x) xs


{- Task -}
qsort :: Ord a => [a] -> [a]
qsort xs | length xs < 2 = xs
qsort xs = let
              pivot = head xs
              less = [x | x <- tail xs, x <= pivot]
              greater = [x | x <- tail xs, x > pivot]
           in (qsort less) ++ [pivot] ++ (qsort greater)


qsort' :: Ord a => [a] -> [a]
qsort' xs | length xs < 2 = xs
qsort' xs = let
              pivot = head xs
              less = filter (<= pivot) (tail xs)
              greater = filter (> pivot) (tail xs)
           in (qsort' less) ++ [pivot] ++ (qsort' greater)


qsort'' :: Ord a => [a] -> [a]
qsort'' [] = []
qsort'' (x:xs) = (qsort'' $ filter (<=x) xs) ++ [x]  ++ (qsort'' $ filter (>x) xs)


{- Task -}
squares'n'cubes :: Num a => [a] -> [a]
squares'n'cubes xs = concat $ map (\(x,y) -> [x,y]) (zip (map (^2) xs) (map (^3) xs))


squares'n'cubes' :: Num a => [a] -> [a]
squares'n'cubes' = concat . map (\x -> [x^2,x^3])


squares'n'cubes'' :: Num a => [a] -> [a]
squares'n'cubes'' = concatMap (\x -> [x^2,x^3])


{- Task -}
rotations :: Int -> [a] -> [[a]]
rotations len xs = take len (iterate (\(y:ys) -> ys ++ [y]) xs)

perms :: [a] -> [[a]]
perms []        = [[]]
perms il@(x:xs) = concatMap ((rotations len).(x:)) (perms xs)
                  where len = length il


{- Task -}
delAllUpper :: String -> String
delAllUpper = unwords . filter (any isLower) . words


{- Task -}
max3 :: Ord a => [a] -> [a] -> [a] -> [a]
max3 xs ys zs = map (\(x,y,z) -> max x $ max y z) (zip3 xs ys zs)


max3' :: Ord a => [a] -> [a] -> [a] -> [a]
max3' = zipWith3 (\a b c -> a `max` b `max` c)


max3'' :: Ord a => [a] -> [a] -> [a] -> [a]
max3'' = zipWith3 (\x y z -> maximum [x,y,z])


{- Task -}
fibStream :: [Integer]
fibStream = 0 : 1 : zipWith (+) fibStream (tail fibStream)


{- Task -}
data Odd = Odd Integer
  deriving (Eq, Show)

instance Enum Odd where
  succ (Odd x) = Odd (x + 2)
  pred (Odd x) = Odd (x - 2)

  fromEnum (Odd x) = fromInteger $ x `div` 2
  toEnum x = Odd (toInteger x * 2 + 1)

  enumFrom = iterate succ
  enumFromThen (Odd x) (Odd y) = map Odd [x, y ..]
  enumFromTo (Odd x) (Odd y) = map Odd [x, x + 2 .. y]
  enumFromThenTo (Odd x) (Odd y) (Odd z) = map Odd [x , y .. z]


{- Folding: Remember -}
{- foldr (-) x [2,1,5] ~> (2 - (1 - (5 - x))) -}
{- foldl (-) x [2,1,5] ~> (((x - 5) - 1) - 2) -}


{- Task -}
meanList :: [Double] -> Double
meanList = (\(x, y) -> x / y) . foldr (\x (s, l) -> (x + s, 1 + l)) (0, 0)


{- Task -}
evenOnly :: [a] -> [a]
evenOnly = fst . foldl (\(s, p) x -> if even p then (s ++ [x], p + 1) else (s, p + 1))  ([], 1)


-- Endless list
evenOnly' :: [a] -> [a]
evenOnly' = foldr (\(n,x) xs -> if even n then x:xs else xs) [] . zip [1, 2..]


maximum' :: (Ord a) => [a] -> a
maximum' = foldl1 max


{- Task -}
lastElem :: [a] -> a
lastElem = foldl1 (\_ y -> y)


lastElem' :: [a] -> a
lastElem' = foldl1 seq


{- Task -}
unfoldr :: (b -> Maybe (a, b)) -> b -> [a]
unfoldr f ini = helper (f ini) where
  helper (Just (x, ini')) = x : unfoldr f ini'
  helper Nothing          = []


revRange :: (Char,Char) -> [Char]
revRange (a, b) = unfoldr g b
  where g = \x -> if x >= a then Just (x, pred x) else Nothing


{- Task -}
data Color = Red | Green | Blue


instance Show Color where
  show Red = "Red"
  show Green = "Green"
  show Blue = "Blue"


{- Task -}
change :: (Ord a, Num a) => a -> [[a]]
change 0 = [[]]
change amount = [c:cs | c <- [2,3,7], amount >= c, cs <- change (amount - c) ]


{- Task -}
data Point = Point Double Double

origin :: Point
origin = Point 0.0 0.0


distanceToOrigin :: Point -> Double
distanceToOrigin (Point x y) = sqrt (x ^ 2 + y ^ 2)


distance :: Point -> Point -> Double
distance (Point x1 y1) (Point x2 y2)= sqrt ((x2 - x1) ^ 2 + (y2 - y1) ^ 2)


{- Task -}
data Shape = Circle Double | Rectangle Double Double

area :: Shape -> Double
area (Circle r) = pi * r ^ 2
area (Rectangle a b) = a * b


{- Task -}
data Result' = Fail' Int | Success'

instance Show Result' where
    show (Fail' c) = "Fail: " ++ show c
    show Success' = "Success"

--doSomeWork' :: SomeData -> Result'
--doSomeWork' d = case doSomeWork d of
--                    (_, 0) -> Success'
--                    (_, c) -> Fail' c


{- Task -}
square :: Double -> Shape
square a = Rectangle a a

isSquare :: Shape -> Bool
isSquare (Rectangle a b) = a == b
isSquare _ = False


{- Constructor -}
data Person = Person String String Int
  deriving Show

firstName :: Person -> String
firstName (Person f _ _) = f


lastName :: Person -> String
lastName (Person _ l _) = l


age :: Person -> Int
age (Person _ _ a) = a


data Person' = Person' { firstName' :: String, lastName' :: String, age' :: Int}
  deriving (Show, Eq)


john = Person' "John" "Smith" 33
johnAge= john & age'


{- Task -}
timeToString :: UTCTime -> String
timeToString = formatTime defaultTimeLocale "%a %d %T"


data LogLevel = Error | Warning | Info


data LogEntry = LogEntry {timestamp :: UTCTime, logLevel :: LogLevel, message :: String}


logLevelToString :: LogLevel -> String
logLevelToString x = case x of
                        Error   -> "Error"
                        Warning -> "Warning"
                        _    -> "Info"

logEntryToString :: LogEntry -> String
logEntryToString l = timeToString (timestamp l) ++ ": " ++ logLevelToString (logLevel l)  ++ ": " ++ message l


{- Task -}
updateLastName :: Person' -> Person' -> Person'
updateLastName p1 p2 = p2 {lastName' = lastName' p1}


name :: Person' -> String
name person = firstName' person ++ " " ++ lastName' person

name' :: Person' -> String
name' (Person' fn ln _) = fn ++ " " ++ ln

name'' :: Person' -> String
name'' (Person' {lastName' = ln, firstName' = fn}) = fn ++ " " ++ ln


{- Task -}
abbrFirstName :: Person' -> Person'
abbrFirstName person@(Person' {firstName' = fn})
    | length fn > 2 = person {firstName' = (++ ".") . take 1 $ fn}
    | otherwise = person


{- Task -}
data Coord a = Coord a a
  deriving Show

getCenter :: Double -> Coord Int -> Coord Double
getCenter size (Coord x y) = Coord (center x) (center y)
  where center c = fromIntegral c * size + size / 2

getCell :: Double -> Coord Double -> Coord Int
getCell size (Coord x y)  = Coord (point x) (point y)
  where point c = floor $ c / size


roots :: Double -> Double -> Double -> Either [Char] (Double, Double)
roots a b c
  | discr >= 0 = Right (x1, x2)
  | otherwise = Left "Negative discriminant"
  where
      x1 = helper . negate $ d
      x2 = helper d
      helper x = (-b + x) / (2 * a)
      d = sqrt discr
      discr = b ^ 2 - 4 * a * c


{- Task -}
findDigit :: [Char] -> Maybe Char
findDigit xs
  | c /= "" = Just (head c)
  | otherwise = Nothing
  where
    c = filter isDigit xs


findDigit' :: [Char] -> Maybe Char
findDigit' [] = Nothing
findDigit' (x:xs) | isDigit x = Just x
                 | otherwise = findDigit' xs


{- Task -}
findDigitOrX :: [Char] -> Char
findDigitOrX xs = case findDigit xs of
                    Just x -> x
                    Nothing -> 'X'


{- Task -}
maybeToList :: Maybe a -> [a]
maybeToList (Just x) = [x]
maybeToList Nothing = []


maybeToList' :: Maybe a -> [a]
maybeToList' x = case x of
                  Just x -> [x]
                  _ -> []

listToMaybe :: [a] -> Maybe a
listToMaybe [] = Nothing
listToMaybe (x:[]) = Just x


listToMaybe' :: [a] -> Maybe a
listToMaybe' xs = case xs of
                    (x:_) -> Just x
                    _ -> Nothing


eitherToMaybe :: Either a a -> Maybe a
eitherToMaybe (Left a) = Just a
eitherToMaybe (Right _) = Nothing


{- strict and lazy calculations -}
data CoordLazy a = CoordLazy a a
  deriving Show

data CoordStrict a = CoordStrict !a !a
  deriving Show

getXLazy :: CoordLazy a -> a
getXLazy (CoordLazy x _) = x

getXStrict :: CoordStrict a -> a
getXStrict (CoordStrict x _) = x


-- getXLazy 3 undefined - Success
-- getXStrict 3 undefined - Fail

{-
import Data.Complex
import Data.Ratio

data Complex a = !a :+ !a
data Ratio a = !a :% !a
-}


{- Recursive data types -}

-- data [] a = [] | a : ([] a)
data List a = Nil | Cons a (List a)
  deriving Show


yz = Cons 'y' (Cons 'z' Nil)


{- Task -}
fromList :: List a -> [a]
fromList Nil = []
fromList (Cons x y) = x : fromList y


toList :: [a] -> List a
toList [] = Nil
toList (x:xs) = Cons x (toList xs)


{- Task -}
data Nat = Zero | Suc Nat
  deriving Show


nat1 = Suc (Suc Zero)
nat2 = Suc (Suc (Suc Zero))


fromNat :: Nat -> Integer
fromNat Zero = 0
fromNat (Suc n) = fromNat n + 1


toNat :: Integer -> Nat
toNat 0 = Zero
toNat n = Suc (toNat (n-1))


add :: Nat -> Nat -> Nat
add x y = toNat $ fromNat x + fromNat y


mul :: Nat -> Nat -> Nat
mul x y = toNat $ fromNat x * fromNat y


fac :: Nat -> Nat
fac x
  | n >= 0 = toNat $ helper 1 n
  | otherwise = error "Argumnet must be >= 0!"
  where
    n = fromNat x
    helper acc 0 = acc
    helper acc n = helper (acc * n) (n-1)


{- Task -}
data Tree a = Leaf a | Node (Tree a) (Tree a)
  deriving Show


height :: Tree a -> Int
height (Leaf _) = 0
height (Node n1 n2) = 1 + max (height n1) (height n2)


size :: Tree a -> Int
size (Leaf _)  = 1
size (Node n1 n2) = 1 + size n1 + size n2


{- Task -}
avg' :: Tree Int -> Int
avg' t =
    let (s,c) = go t
    in s `div` c
  where
    go :: Tree Int -> (Int,Int)
    go (Leaf n) = (n, 1)
    go (Node n1 n2) = (sum1 + sum2, count1 + count2)
      where
        (sum1, count1) = go n1
        (sum2, count2) = go n2


type AssociativeList k v = [(k, v)]


lookUp :: Eq k => k -> AssociativeList k v -> Maybe v
lookUp _ [] = Nothing
lookUp key ((x,y):xys)
  | key == x = Just y
  | otherwise = lookUp key xys
  
  
newtype IntList = IList [Int]
  deriving Show


newtype Identity a = Identity {runIdentity :: a}
  deriving (Eq, Ord, Show)


-- Monoid --
class Monoid a where
  mempty :: a             -- Neutral element
  mappend :: a -> a -> a  -- Operation that introduced on the elements of the monoid

  mconcat :: [a] -> a
  mconcat = foldr mappend mempty


{- Example with List -}
instance Monoid [a] where
  mempty = []
  mappend = (++)


{- Addition implementation for monoid -}
newtype Sum a = Sum {getSum :: a}
  deriving (Eq, Show, Read, Bounded, Ord)
  
instance Num a => Monoid (Sum a) where
  mempty = Sum 0
  (Sum x) `mappend` (Sum y) = Sum (x + y)


{- Product implementation for monoid -}
newtype Product a = Product {getProduct :: a}
  deriving (Eq, Show, Read, Bounded, Ord)

instance Num a => Monoid (Product a) where
  mempty = Product 1
  (Product x) `mappend` (Product y) = Product (x * y)


{- Task -}
xor :: Bool -> Bool -> Bool
x `xor` y = (x || y) && (not (x && y))

newtype Xor = Xor { getXor :: Bool }
  deriving (Eq, Show)

instance Monoid Xor where
  mempty = Xor False
  (Xor x) `mappend` (Xor y) = Xor (x `xor` y)

{- Monoid Pairs (tuple) -}
instance (Monoid a, Monoid b) => Monoid (a,b) where
  mempty = (mempty, mempty)
  (x1,x2) `mappend` (y1,y2) = (x1 `mappend` y1, x2 `mappend` y2)

{- Example: ("ABC", Product 1) `mappend` ("CDE", Product 3) -}


{- Monoid Maybe -}
instance Monoid a => Monoid (Maybe a) where
  mempty = Nothing
  Nothing `mappend` m = m
  m `mappend` Nothing = m
  Just m1 `mappend` Just m2 = Just (m1 `mappend` m2)


newtype First a = First { getFirst :: Maybe a }
  deriving (Eq, Ord, Read, Show)

instance Monoid (First a) where
  mempty = First Nothing
  First Nothing `mappend` r = r
  l `mappend` _             = l


{- mconcat $ map First [Nothing, Just 3, Just 5] -}
firstConcat = getFirst . mconcat . map First



-- MONAD --

{- Uncomment to check (Data.Functor) -}
--class Functor f where
--  fmap :: (a -> b) -> (f a -> f b)
--
--instance Functor [] where
--  fmap = map
--
--instance Functor Maybe where
--  fmap _ Nothing = Nothing
--  fmap f (Just a) = Just (f a)


{- Task -}
data Point3D a = Point3D a a a
  deriving Show

instance Functor Point3D where
  fmap f (Point3D x1 x2 x3)= Point3D (f x1) (f x2) (f x3)


{- Task -}
data GeomPrimitive a = Point' (Point3D a) | LineSegment (Point3D a) (Point3D a)

instance Functor GeomPrimitive where
  fmap f (Point' p) = Point' (fmap f p)
  fmap f (LineSegment p1 p2) = LineSegment (fmap f p1) (fmap f p2)


{- Binary trees -}
-- SOOO BEAUTIFULL !!!
data Tree' a = Leaf' a | Branch (Tree' a) a (Tree' a)
  deriving Show

testTree = Branch (Leaf' 2) 3 (Branch (Leaf' 4) 5 (Leaf' 6))

instance Functor Tree' where
  fmap g (Leaf' x)       = Leaf' (g x)
  fmap g (Branch l x r) = Branch (fmap g l) (g x) (fmap g r)

{- fmap == (<$>) -}
{- (^2) <$> testTree -}
{- (^2) `fmap` testTree -}

-- Operator (<$) replace all values in data structure


{- Task -}
data Tree'' a = Leaf'' (Maybe a) | Branch' (Tree'' a) (Maybe a) (Tree'' a)
  deriving Show


instance Functor Tree'' where
  fmap g (Leaf'' x)       = case x of
                                Just x  ->  Leaf'' (Just (g x))
                                _       ->  Leaf'' (Nothing)
  fmap g (Branch' l x r)  = case x of
                                Just x  ->  Branch' (fmap g l) (Just (g x)) (fmap g r)
                                _       ->  Branch' (fmap g l) (Nothing) (fmap g r)



--instance Functor ((,), s) where
--  fmap g (x,y) = (x, g y)

-- (a -> b) -> (,) s a -> (,) s b
-- (a -> b) -> (s,a) -> (s,b)
-- fmap succ (1, 'A')


--instance Functor (Either e) where
--  fmap _ (Left x) = Left x
--  fmap g (Right y) = Right (g y)

-- (a -> b) -> Either e a -> Either e b
-- fmap (+3) $ Right 1  ->  Right 4
-- fmap (+3) $ Left 2   ->  Left 2


{- Task -}
data Entry k1 k2 v = Entry (k1, k2) v
  deriving Show

data Map k1 k2 v = Map [Entry k1 k2 v]
  deriving Show


instance Functor (Entry k1 k2) where
  fmap g (Entry (k1, k2) v) = Entry (k1, k2) (g v)


instance Functor (Map k1 k2) where
  fmap g (Map xs) = Map (map (fmap g) xs)


{- Rules for Functors -}
-- 1. fmap id = id
-- 2. fmap (f . g) = fmap f . fmap g


{- 
 -
 - Functions with effects 
 -
 - f :: a -> Maybe b       -- can sometimes fail (Just b `or` Nothing)
 - f :: a -> [b]           -- can return many results
 - f :: a -> (Either s) b  -- can sometimes end with a typed exception
 - f :: a -> (s,b)         -- can write to the log
 - f :: a -> ((->) e) b    -- can read from external environments
 - f :: a -> (State s) b   -- work with a named state
 - f :: a -> IO a          -- carry out inputs/outputs (files, console ...)
 -
-}


{- Task -}
data Log a = Log [String] a
  deriving Show

add1Log = toLogger (+1) "added one"
mult2Log = toLogger (*2) "multiplied by 2"

toLogger :: (a -> b) -> String -> (a -> Log b)
toLogger f msg = Log [msg] . f


execLoggers :: a -> (a -> Log b) -> (b -> Log c) -> Log c
execLoggers x f g = Log (msg ++ msg') res'
  where
    Log msg res = f x
    Log msg' res' = g res


{-
 - class Monad m where
 -    return :: a -> m a
 -    (>>=) :: m a -> (a -> m b) -> m b   -- bind
 -    ...
 -
 - infixl 1 >>=
 - return True :: [] Bool       ~> [True]
 - return True :: Maybe Bool    ~> [Just True]
-}


toKleisli :: Monad m => (a -> b) -> (a -> m b)
toKleisli f x = return (f x)
-- toKleisli f = \x -> return (f x)
-- toKleisli f = return .

-- toKleisli cos 0 :: [Double]    ~> [1.0]
-- toKleisli cos 0 :: IO Double   ~> 1.0


{- Task -}
returnLog :: a -> Log a
returnLog x = Log [] x


{- Task -}
bindLog :: Log a -> (a -> Log b) -> Log b
bindLog (Log msg x) f =  Log (msg ++ msg') res
  where Log msg' res = f x


{- Task -}
instance Functor Log where
  fmap = liftM

instance Applicative Log where
  pure = return
  (<*>) = ap

instance Monad Log where
    return = returnLog
    (>>=) = bindLog


execLoggersList :: a -> [a -> Log a] -> Log a
execLoggersList x ys = foldl (>>=) (return x) ys


{-
 - class Monad m where
 -    return :: a -> m a
 -    (>>=) :: m a -> (a -> m b) -> m b   -- bind
 -    (>>) :: m a -> m b -> m b   -- bind
 -    x >> y = x >>= \_ -> y
 -    fail :: String -> m a
 -    fail s = error s
-}


-- Control.Monad
(=<<) :: Monad m => (a -> m b) -> m a -> m b
(=<<) = flip (>>=)


-- Fish
(<=<) :: Monad m => (b -> m c) -> (a -> m b) -> (a -> m c)
f <=< g = \x -> g x >>= f


{- Task -}
-- instance Functor SomeType where
--   fmap f x = x >>= (return . f)



-- `do` notation --

-- do { e1 ; e2 }           == e1 >> e2
-- do { p <- e1 ; e2 }      == e1 >>= \p -> e2
-- do { let v = e1 ; e2 }   == let v = e1 in do e2


-- Monad Identity
instance Monad Identity where
  return x = Identity x
  Identity x >>= k = k x

instance Functor Identity where
  fmap  f (Identity x) = Identity (f x)

instance Applicative Identity where
  pure x = Identity x
  Identity f <*> Identity v = Identity (f v) 

wrap'n'succ :: Integer -> Identity Integer
wrap'n'succ x = Identity (succ x)


-- Monad Maybe
type Name = String
type Database = [(Name, Name)]

fathers, mothers :: Database
fathers = [("Bill", "John"),
           ("Ann", "John"),
           ("John", "Piter")]

mothers = [("Bill", "Jane"),
           ("Ann", "Jane"),
           ("John", "Alice"),
           ("Jane", "Dorothy"),
           ("Alice", "Mary")]

getM, getF :: Name -> Maybe Name
getM person = lookup person mothers
getF person = lookup person fathers


-- getF "Bill" >>= getM >>= getM
-- do {f <- getF "Bill"; gm <- getM f; getM gm}


granmas :: Name -> Maybe (Name, Name)
granmas person = do
  m <- getM person
  gmm <- getM m
  f <- getF person
  gmf <- getM f
  return (gmm, gmf)


{- Task -}
data Token = Number Int | Plus | Minus | LeftBrace | RightBrace 
  deriving (Eq, Show)

asToken :: String -> Maybe Token
asToken x | all isDigit x = Just (Number (read x :: Int))
asToken x = case x of
                "+"       ->  Just Plus
                "-"       ->  Just Minus
                "("       ->  Just LeftBrace
                ")"       ->  Just RightBrace
                _         ->  Nothing


tokenize :: String -> Maybe [Token]
tokenize input = sequence (map asToken (words input))


-- Monad List

-- return 4 :: [Int] ~> [4]
-- [1,2] >>= (\x -> [x,x,x]) ~> [1,1,1,2,2,2]
-- concat $ map (\x -> [x,x,x]) [1,2] ~> [1,1,1,2,2,2]

list = [(x,y) | x <- [1,2,3], y <- [4,5,6]]

list' = do
  x <- [1,2,3]
  y <- [4,5,6]
  return (x,y)

list'' =
  [1,2,3] >>= (\x ->
  [4,5,6] >>= (\y ->
  return (x,y)))

lst = [(x,y) | x <- [1,2,3], y <- [1,2], x /= y]


{- Task -}
-- nextPositionsN :: Board -> Int -> (Board -> Bool) -> [Board]
-- nextPositionsN b 0 pred | pred b = [b]
-- nextPositionsN _ n _    | n <= 0  = []
-- nextPositionsN b n pred = do
--   p <- nextPositions b
--     nextPositionsN p (n-1) pred


lst' = do
  x <- [1,2,3]
  y <- [1,2]
  True <- return (x /= y)
  return (x,y)

lst'' = 
  [1,2,3]         >>= (\x ->
  [1,2]           >>= (\y ->
  return (x /= y) >>= (\b ->
  case b of True -> return (x,y)
            _    -> fail "...")))

lst''' = do 
  x <- [1,2,3]
  y <- [1,2]
  if x /= y then "E" else []
  return (x,y)


{- Task -}
pythagoreanTriple :: Int -> [(Int, Int, Int)]
pythagoreanTriple x = do 
  a <- [1..x]
  b <- [1..x]
  c <- [1..x]
  if a^2 + b^2 == c^2 && a < b then "E" else []
  return (a,b,c)

-- Monad IO

-- type IO a = IO (RealWorld -> (RealWorld, a))
-- return :: a -> IO a ~> return a = \w -> (w,a)
-- (>>=) :: IO a -> (a -> IO b) -> IO b

-- instance Monad IO where
--  return a = \w -> (w,a)
--  (>>=) m k = \w -> case m w of (w',a) -> k a w'


getLine' :: IO String
getLine' = do
  c <- getChar
  if c == '\n' then
    return []
  else do
    cs <- getLine'
    return (c:cs)

putStr' :: String -> IO ()
putStr' [] = return ()
putStr' (x:xs) = putChar x >> putStr' xs


{-
import Control.Monad

sequence_ :: Monad m => [m a] -> m ()
sequence_ = foldr (>>) (return ())

-}


putStr'' :: String -> IO ()
putStr'' = sequence_ . map putChar


{-
mapM_ :: Monad m => (a -> m b) -> [a] -> m ()
mapM_ f = sequence_ , map f
-}


putStr''' :: String -> IO ()
putStr''' = mapM_ putChar
