data Sum b a =
    First a
  | Second b

instance Functor (Sum b) where
  fmap f (First a) = First (f a)
  fmap f (Second b) = Second b

data Company a c b =
    DeepBlue a c
  | Something b

instance Functor (Company a c) where
  fmap f (DeepBlue a c) = DeepBlue a c
  fmap f (Something b) = Something (f b)

data More b a =
    L a b a
  | R b a b

instance Functor (More b) where
  fmap f (L a b a') = L (f a) b (f a')
  fmap f (R b a b') = R b (f a) b'

data Quant a b =
    Finance
  | Desk a
  | Bloor b

instance Functor (Quant a) where
  fmap f Finance = Finance
  fmap f (Desk a) = Desk a
  fmap f (Bloor b) = Bloor (f b)

data K a b = K a

instance Functor (K a) where
  fmap f (K a) = K a

data EvilGoateeConst a b = GoatyConst b

instance Functor (EvilGoateeConst a) where
  fmap f (GoatyConst b) = GoatyConst (f b)

data LiftInOut f a = LiftInOut (f a) deriving (Eq, Show)
instance Functor f => Functor (LiftInOut f) where
  fmap f (LiftInOut fa) = LiftInOut (fmap f fa)

data Parappa f g a = DaWrappa (f a) (g a)
instance (Functor f, Functor g) => Functor(Parappa f g) where
  fmap x (DaWrappa (fa) (ga)) = DaWrappa (fmap x fa) (fmap x ga)

data IgnoreOne f g a b = IgnoringSomething (f a) (g b)
instance Functor g => Functor (IgnoreOne f g a) where
  fmap f (IgnoringSomething (fa) (gb)) = IgnoringSomething fa (fmap f gb)


data Notorious g o a t = Notorious (g o) (g a) (g t)
instance Functor g => Functor(Notorious g o a) where
  fmap f (Notorious go ga gt) = Notorious go ga (fmap f gt)

data ContingGood a = Heisenberg Int a deriving (Eq, Show)
instance Functor (ContingGood) where
  fmap f (Heisenberg n a) = Heisenberg n (f a)

data List a =
    Nil
  | Cons a (List a)

instance Functor List where
  fmap _ Nil = Nil
  fmap f (Cons a la) = Cons (f a) (fmap f la)

data GoatLord a =
    NoGoat
  | OneGoat a
  | MoreGoats (GoatLord a) (GoatLord a) (GoatLord a)

data Test a = Test a

instance Functor GoatLord where
  fmap _ NoGoat = NoGoat
  fmap f (OneGoat a) = OneGoat (f a)
  fmap f (MoreGoats g1 g2 g3) = MoreGoats (fmap f g1) (fmap f g2) (fmap f g3)




