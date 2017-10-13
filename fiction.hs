-- data Fiction = Fiction deriving Show
-- data Nonfiction = Nonfiction deriving Show
-- data BookType =   FictionBook Fiction
--                 | NonfictionBook Nonfiction
--                 deriving Show

-- type AuthorName = String
-- data Author = Author (AuthorName, BookType)

data Fiction = Fiction deriving Show
data Nonfiction = Nonfiction deriving Show

type AuthorName = String
data Author =   FictionAuthor AuthorName
              | NonfictionAuthor AuthorName
              deriving Show


-- data FlowerType =   Gardenia
--                   | Daisy
--                   | Rose
--                   | Lilac
--                   deriving Show

type Gardener = String

data Garden =   Gardenia Gardener
              | Daisy Gardener
              | Rose Gardener
              | Lilac Gardener
              deriving Show