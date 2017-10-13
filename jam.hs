module Jammin where
  import Data.List

  data Fruit =
      Peach
    | Plum
    | Apple
    | Blackberry
    deriving (Eq, Show, Ord)

  data JamJars =
    Jam {fruit :: Fruit, jars :: Int}
    deriving (Eq, Show, Ord)

  row1 = Jam {fruit = Plum, jars = 4}
  row2 = Jam Peach 1
  row3 = Jam Plum 4
  row4 = Jam Blackberry 8
  row5 = Jam Apple 4
  row6 = Jam Apple 7

  allJam = [row1, row2, row3, row4, row5, row6]

  rowJars :: [JamJars] -> [Int]
  rowJars = map jars

  jarsCount :: [JamJars] -> Int
  jarsCount = sum . rowJars

  mostRow :: [JamJars] -> JamJars
  mostRow = maximumBy (\x -> \y -> compare (fruit x) (fruit y))

  compareKind :: JamJars -> JamJars -> Ordering
  compareKind j1 j2 = compare (fruit j1) (fruit j2)

  sortJams :: [JamJars] -> [JamJars]
  sortJams = sortBy compareKind

  groupJam :: [JamJars] -> [[JamJars]]
  groupJam = groupBy (\x -> \y -> fruit x == fruit y)