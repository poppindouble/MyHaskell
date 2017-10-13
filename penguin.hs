module Pattern where

data WherePenguinsLive =
    Galapagos
  | Antarctica
  | Australia
  | SouthAfrica
  | SouthAmerica
  deriving (Eq, Show)

data Penguin =
  Peng WherePenguinsLive
  deriving (Eq, Show)

isSouthAfrica :: WherePenguinsLive -> Bool
isSouthAfrica SouthAfrica = True
isSouthAfrica _ = False

gimmeWhereTheyLive :: Penguin -> WherePenguinsLive
gimmeWhereTheyLive (Peng location) = location

australiaPenguin :: Penguin -> Bool
australiaPenguin (Peng Australia) = True
australiaPenguin _ = False

southAmericaPenguin :: Penguin -> Bool
southAmericaPenguin (Peng SouthAmerica) = True
southAmericaPenguin _ = False

australiaOrSouthAmericaPenguin :: Penguin -> Bool
australiaOrSouthAfricaPenguin (Peng Australia) = True
australiaOrSouthAfricaPenguin (Peng SouthAmerica) = True
australiaOrSouthAfricaPenguin (Peng _) = False