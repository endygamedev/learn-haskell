module Calendar where

import Prelude (Int, Char, String, Show(..), (++))


main = undefined


-- Date
data Date = Date Year Month Day

-- Year
data Year = Year Int

-- Month
data Month = January | February | March | April
			| May | June | July | August
			| September | October | November | December

-- Day
data Day = Day Int

-- Week
data Week = Monday | Tuesday | Wednesday
			| Thursday | Friday | Saturday
			| Sunday

instance Show Week where
  show Monday		= "Mon"
  show Tuesday	= "Tue"
  show Wednesday 	= "Wed"
  show Thursday 	= "Thu"
  show Friday		= "Fri"
  show Saturday	= "Sat"
  show Sunday 	= "Sun"


-- Время
data Time = Time Hour Minute Second

data Hour = Hour Int	-- hours
data Minute = Minute Int	-- minutes
data Second = Second Int	-- seconds


instance Show Time where
  show (Time h m s) = show h ++ ":" ++ show m ++ ":" ++ show s


instance Show Hour where
  show (Hour h) = addZero (show h)

instance Show Minute where
  show (Minute m) = addZero (show m)

instance Show Second where
  show (Second s) = addZero (show s)


addZero :: String -> String
addZero (a:[]) = '0' : a : []
addZero as = as