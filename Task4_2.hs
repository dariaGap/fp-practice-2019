module Task4_2 where

{-
  Задание 4.1
  Реализация монады над множеством из четырёх элементов.
  Реализуйте все требуемые классы типов и объясните, почему они реализованы именно так.
-}

data FourOf a = FourOf {e1 :: a, e2 :: a, e3 :: a, e4 :: a} deriving(Show,Eq)

-- реализуйте классы `Functor`, `Applicative` и `Monad` для типа `FourOf`
-- таким образом, что
-- do { x <- FourOf 1 2 3 4; y <- FourOf 4 6 7 8; return $ x + y } === FourOf 5 8 10 12

instance Functor FourOf where
    fmap f (FourOf x1 x2 x3 x4) = FourOf (f $ x1) (f $ x2) (f $ x3) (f $ x4)

instance Applicative FourOf where
    pure x = FourOf x x x x
    (<*>) (FourOf f1 f2 f3 f4) (FourOf x1 x2 x3 x4) = FourOf (f1 $ x1) (f2 $ x2) (f3 $ x3) (f4 $ x4)

instance Monad FourOf where
-- Так как f возвращает FourOf, то есть, 4 элемента для каждого x, 
-- то выбираем из FourOf для каждого x один элемент с соответствующим номером с помощью e1, e2, e3, e4
    (>>=) (FourOf x1 x2 x3 x4) f = FourOf (e1(f x1)) (e2(f x2)) (e3(f x3)) (e4(f x4))