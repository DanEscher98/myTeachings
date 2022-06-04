module FirstMonads where

instance Show Peano where
    show Zero  = "0"
    show (S n) = "S" ++ show n

data Identidad a = Id a

instance Show a => Show (Identidad a) where
    show (Id a) = show a

instance Functor Identidad where
    fmap :: (a -> b) -> Identidad a -> Identidad b
    fmap f (Id x) = Id (f x)

instance Applicative Identidad where
    pure :: a -> Identidad a
    pure x = Id x
    (<*>) :: Identidad (a -> b) -> Identidad a -> Identidad b
    (Id f) <*> (Id x) = Id (f x)

instance Monad Identidad where
    (>>=) :: Identidad a -> (a -> Identidad b) -> Identidad b
    (Id x) >>= f = f x


data Quizas a = Nada | Un a

instance Show a => Show (Quizas a) where
    show Nada   = ":'v"
    show (Un x) = ":v/ " ++ show x

instance Functor Quizas where
    fmap :: (a -> b) -> Quizas a -> Quizas b
    fmap _ Nada   = Nada
    fmap f (Un x) = Un (f x)

instance Applicative Quizas where
    pure x = Un x
    Nada <*> _        = Nada
    _ <*> Nada        = Nada
    (Un f) <*> (Un x) = Un (f x)

instance Monad Quizas where
    (>>=) :: Quizas a -> (a -> Quizas b) -> Quizas b
    Nada >>= _   = Nada
    (Un x) >>= f = f x

data Peano = Zero | S Peano

suma :: Peano -> Peano -> Quizas Peano
suma Zero n  = Un n
suma (S m) n = fmap S (suma m n)

mult :: Peano -> Peano -> Quizas Peano
mult Zero _  = Un Zero
mult (S m) n = (mult m n) >>= (flip suma n)

rest :: Peano -> Peano -> Quizas Peano
rest m Zero      = Un m
rest Zero n      = Nada
rest (S m) (S n) = rest m n

res :: Peano -> Peano -> Quizas Peano
res _ Zero = Nada
res m n = case (rest m n) of
            Nada    -> Un m
            (Un m') -> res m' n

qot :: Peano -> Peano -> Quizas Peano
qot _ Zero = Nada
qot m n = case (rest m n) of
            Nada    -> Un Zero
            (Un m') -> S <$> (qot m' n)
