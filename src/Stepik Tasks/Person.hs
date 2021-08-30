module Person where

import Prelude


s1 = "firstName = John\nlastName = Connor\nage = 30"
s2 = "firstName = John Smith\nlastName = Connor\nage = 30\nasde=as11"
s3 = "firstName=Barbarian\nlastName=Conn On\nage=30"


data Error = ParsingError | IncompleteDataError | IncorrectDataError String
  deriving Show

data Person = Person { firstName :: String, lastName :: String, age :: Int }
  deriving Show


split :: Eq a => a -> [a] -> [[a]]
split d [] = []
split d xs = x : split d (drop 1 y)
        where (x,y) = span (/= d) xs


parsingError :: String -> [[String]]
parsingError s
    | not $ all (== True) [if length xs == 3 then (xs !! 1 == "=") else False | xs <- xss] = xss
    | otherwise = error "Not this situation!"
        where xss = map words . split '\n' $ s


parsePerson :: String -> Either Error Person
parsePerson = undefined