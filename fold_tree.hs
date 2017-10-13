data BinaryTree a =   Leaf
                    | Node (BinaryTree a) a (BinaryTree a)
                    deriving (Eq, Ord, Show)

-- testTree' :: BinaryTree Integer
-- testTree' =
--   Node
--   (
--     Node
--       (Node Leaf 4 Leaf)
--       2
--       (Node Leaf 5 Leaf)
--   )
--   1
--   (Node Leaf 3 Leaf)


testTree' :: BinaryTree Integer
testTree' =
  Node
  (Node Leaf 2 Leaf)
  1
  (Node Leaf 3 Leaf)


myFoldRight :: (a -> b -> b) -> b -> [a] -> b
myFoldRight f init [] = init
myFoldRight f init (x : xs) = f x (myFoldRight f init xs)

foldTree :: (a -> b -> b -> b) -> b -> BinaryTree a -> b
foldTree f init Leaf = init
foldTree f init (Node left a right) = f a (foldTree f init left)  (foldTree f init right)

flattenIn :: BinaryTree a -> [a] -> [a]
flattenIn Leaf l = l
flattenIn (Node left a right) l = a : (flattenIn left (flattenIn right l))