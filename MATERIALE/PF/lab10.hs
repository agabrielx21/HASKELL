import Prelude hiding (Functor, fmap)

class Functor f where
  fmap :: (a -> b) -> f a -> f b

newtype Identity a = Identity a
    deriving Show

instance Functor Identity where
    fmap f (Identity a) = Identity (f a)

data Pair a = Pair a a
    deriving Show


instance Functor Pair where
    fmap f (Pair b c) = Pair (f b) (f c)

data Constant a b = Constant b
    deriving Show


instance Functor (Constant a) where
    fmap f (Constant b) = Constant (f b)

data Two a b = Two a b
    deriving Show


instance Functor (Two a) where
    fmap f (Two x y) = Two (x) (f y)  

data Three a b c = Three a b c
    deriving Show

instance Functor (Three x y) where
    fmap f (Three a b c) = Three a b (f c)

data Three' a b = Three' a b b
    deriving Show

instance Functor (Three' a) where
    fmap f (Three' a b c) = Three' a (f b) (f b)

data Four a b c d = Four a b c d
    deriving Show

instance Functor (Four a b c) where
    fmap f (Four a b c d) = Four a b c (f d)

data Four'' a b = Four'' a a a b
    deriving Show

instance Functor (Four'' a) where
    fmap f (Four'' a x y b) = Four'' a a a (f b)

data Quant a b = Finance | Desk a | Bloor b
    deriving Show

instance Functor (Quant a) where
    fmap f (Desk a) = (Desk ( a ))
    fmap f (Bloor b) = Bloor (f b)
    fmap f Finance = Finance     

data LiftItOut f a = LiftItOut (f a)
    deriving Show

instance Functor f => Functor (LiftItOut f) where
    fmap f (LiftItOut g) = LiftItOut (fmap f g)

data Parappa f g a = DaWrappa (f a) (g a)
    deriving Show

instance (Functor f, Functor g) => Functor (Parappa f g) where
    fmap g (DaWrappa x y) = DaWrappa(fmap g x) (fmap g y)

data IgnoreOne f g a b = IgnoringSomething (f a) (g b)
    deriving Show

instance (Functor f, Functor g) => Functor(IgnoreOne f g a) where
    fmap h (IgnoringSomething a b) = IgnoringSomething a (fmap h b)

data Notorious g o a t = Notorious (g o) (g a) (g t)
    deriving Show

instance (Functor f) => Functor(Notorious f g h) where
    fmap h (Notorious a b c) = Notorious(a) (b) (fmap h c)


data GoatLord a = NoGoat | OneGoat a | MoreGoats (GoatLord a) (GoatLord a) (GoatLord a)
    deriving Show
instance Functor(GoatLord) where
    fmap f NoGoat = NoGoat
    fmap f (OneGoat a) = OneGoat (f a)
    fmap f (MoreGoats a b c) = MoreGoats (fmap f a) (fmap f b) (fmap f c)

data TalkToMe a = Halt | Print String a | Read (String -> a)

instance Functor(TalkToMe) where
    fmap f Halt = Halt
    fmap f (Print a b) = Print a (f b)
    fmap f (Read x) = Read ( f . x )