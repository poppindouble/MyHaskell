type Name = String
type Age = Integer

data Person = Person Name Age deriving Show

data PersonInvalid =
       NameEmpty
     | AgeTooLow
     | PersonInvalidUnknown String
     deriving (Eq, Show)

data BinaryTree a = Leaf | Node (BinaryTree a) a (BinaryTree a)

mkPerson :: Name -> Age -> Either PersonInvalid Person
mkPerson name age
  | name /= "" && age > 0 = Right $ Person name age
  | name == "" = Left NameEmpty
  | not (age > 0) = Left AgeTooLow
  | otherwise =
      Left $ PersonInvalidUnknown $
        "Name was: " ++ show name ++
        " Age was: " ++ show age

gimmePerson :: IO ()
gimmePerson = do
  putStr "age: "
  age <- getLine
  putStr "name: "
  name <- getLine
  case mkPerson name (read age :: Integer) of
    Right p -> putStrLn $ "created " ++ show p
    Left NameEmpty -> putStrLn "Empty name"
    Left AgeTooLow -> putStrLn "Age npot valid"
    Left (PersonInvalidUnknown err) -> putStrLn err