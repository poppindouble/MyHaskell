module Arith4 where

roundTrip :: (Show a, Read a) => a -> a
roundTrip a = read . show $ a

roundTripPointFree :: (Show a, Read a) => a -> a
roundTripPointFree = read . show

-- read :: Read a => String -> a
-- show :: Show a => a -> String

roundTripTest :: (Show a, Read b) => a -> b
roundTripTest = read . show

main = do
  print (roundTrip 4)
  print (id 4)

  print (roundTripPointFree 5)