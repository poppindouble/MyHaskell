data BinaryTree a =   Leaf
                    | Node (BinaryTree a) a (BinaryTree a)
                    deriving (Eq, Ord, Show)

testTree' :: BinaryTree Integer
testTree' = Node
  (Node Leaf 3 Leaf)
  1
  (Node Leaf 4 Leaf)

mapExpected :: BinaryTree Integer
mapExpected = Node
  (Node Leaf 4 Leaf)
  2
  (Node Leaf 5 Leaf)

mapTree :: (a -> b) -> BinaryTree a -> BinaryTree b
mapTree _ Leaf = Leaf
mapTree f (Node left a right) = Node (mapTree f left) (f a) (mapTree f right)

result = mapTree (+1) testTree' == mapExpected