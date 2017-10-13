data DogueDeBordeaux doge = DogueDeBordeaux doge
data Doggies a = Husky a | Mastiff a

data Size = Size Integer deriving (Eq, Show)

data Price = Price Integer
               deriving (Eq, Show)

data Manufacturer = Mini | Mazda | Tata
                      deriving (Eq, Show)

data Airline = PapuAir | CatapultsR'Us | TakeYourChancesUnited
                 deriving (Eq, Show)

data Vehicle =   Car Manufacturer Price
               | Plane Airline Size
                 deriving (Eq, Show)

myCar = Car Mini (Price 14000)
urCar = Car Mazda (Price 2000)
clownCar = Car Tata (Price 10000)
doge = Plane PapuAir (Size 100)

isCar :: Vehicle -> Bool
isCar (Car _ _)  = True
isCar _ = False

isPlane :: Vehicle -> Bool
isPlane (Plane _ _) = True
isPlane _ = False

areCars :: [Vehicle] -> [Bool]
areCars xs = map isCar xs

getManu :: Vehicle -> Manufacturer
getManu (Car manu _) = manu

class TooMany a where
  tooMany :: a -> Bool

instance TooMany Int where
  tooMany n = n > 42

newtype Goats = Goats Int
                  deriving Show

instance TooMany Goats where
  tooMany (Goats n) = n > 55