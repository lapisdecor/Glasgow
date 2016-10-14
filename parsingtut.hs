module Main where
import ShowParser ( parseShow )

data PersonRecord = MkPersonRecord {
  name :: String,
  address :: Address,
  id :: Integer,
  labels :: [Label]
} deriving (Show)

data Address = MkAddress {
  line1 :: String,
  number :: Integer,
  street :: String,
  town :: String,
  postcode :: String
} deriving (Show)

data Label = Red | Blue | Green | Yellow deriving (Show)

rec1 = MkPersonRecord
    "Ana Compulina"
    (MkAddress "Jupiter Rising Planetary" 89 "Milky Way Av." "Daisy Town" "400 ESP")
    9893892
    [Green, Red]

rec2 = MkPersonRecord
    "John Smith"
    (MkAddress "Central Biology Lab" 37 "Anibal Jones Street" "Daisy Town" "393 POR")
    4923932
    [Blue, Yellow]


-- main = putStrLn $ show [rec1, rec2]

rec_str = show [rec1, rec2]
main =  putStrLn $ parseShow rec_str
