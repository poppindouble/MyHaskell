data BinaryTree a =
    Leaf
  | Node (BinaryTree a) a (BinaryTree a)
  deriving (Eq, Show)

testTree :: BinaryTree Int
testTree =
  Node
  (
    Node
      (Node Leaf 4 Leaf)
      2
      (Node Leaf 5 Leaf)
  )
  1
  (Node Leaf 3 Leaf)


unfold :: (a -> Maybe (a, b, a)) -> a -> BinaryTree b
unfold f a = case f a of
               Nothing -> Leaf
               Just (x, y, z) -> Node (unfold f x) y (unfold f z)

-- treeBuild :: Integer -> BinaryTree Integer
-- treeBuild n = go (unfold (\a -> Just (a+1, a, a+1)) 0) n
--   where
--     go :: BinaryTree Integer -> Integer -> BinaryTree Integer
--     go (Node left a right) n
--       | n == 0 = Le
--       | otherwise = go (Node left a right) (n-1)

treeBuild :: Integer -> BinaryTree Integer
treeBuild n = unfold f 0
  where
    f :: Integer -> Maybe (Integer, Integer, Integer)
    f a
      | a == n = Nothing
      | otherwise = Just (a+1, a, a+1)