data Bright = Blue | Red deriving (Read, Show)

darkBright :: Bright -> Bool
darkBright Blue = True
darkBright Red = False

lightenBright :: Bright -> Bright
lightenBright Blue = Red
lightenBright Red = Red

data Pastel = Turquoise | Tan deriving (Read, Show)

darkPastel :: Pastel -> Bool
darkPastel Turquoise = True
darkPastel Tan = False

lightenPastel :: Pastel -> Pastel
lightenPastel Turquoise = Tan
lightenPastel Tan = Tan

class Color a where
  dark :: a -> Bool
  lighten :: a -> a

instance Color Bright where
  dark = darkBright
  lighten = lightenBright

instance Color Pastel where
  dark = darkPastel
  lighten = lightenPastel

data Foo = Bar | Baz

instance Show Foo where
  show Bar = "it is a Bar"
  show Baz = "this is a Baz"

data Foo2 = Bar2 | Baz2 deriving (Show, Read)
  
