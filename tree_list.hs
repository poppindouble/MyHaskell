data BinaryTree a =
    Leaf
  | Node (BinaryTree a) a (BinaryTree a)
  deriving (Eq, Ord, Show)

testTree' :: BinaryTree Integer
testTree' =
  Node
  (
    Node
      (Node Leaf 4 Leaf)
      2
      (Node Leaf 5 Leaf)
  )
  1
  (Node Leaf 3 Leaf)

preorder' :: BinaryTree a -> [a]
preorder' Leaf = []
preorder' (Node left a right) = a : (preorder' left ++ preorder' right)

inorder' :: BinaryTree a -> [a]
inorder' Leaf = []
inorder' (Node left a right) = (inorder' left) ++ (a : inorder' right)

postorder' :: BinaryTree a -> [a]
postorder' Leaf = []
postorder' (Node left a right) = postorder' left ++ postorder' right ++ [a]


preorder :: BinaryTree a -> [a]
preorder Leaf = []
preorder (Node left a right) = go (Node left a right) []
  where
    go :: BinaryTree a -> [a] -> [a]
    go (Node Leaf a Leaf) result = [a]
    go (Node left a right) result = (a : result) ++ (go left result) ++ (go right result)

inorder :: BinaryTree a -> [a]
inorder Leaf = []
inorder (Node left a right) = go (Node left a right) []
  where
    go :: BinaryTree a -> [a] -> [a]
    go (Node Leaf a Leaf) result = [a]
    go (Node left a right) result = (go left result) ++ (a : result) ++ (go right result)

postorder :: BinaryTree a -> [a]
postorder Leaf = []
postorder (Node left a right) = go (Node left a right) []
  where
    go :: BinaryTree a -> [a] -> [a]
    go (Node Leaf a Leaf) result = [a]
    go (Node left a right) result = (go left result) ++ (go right result) ++ (a : result)

