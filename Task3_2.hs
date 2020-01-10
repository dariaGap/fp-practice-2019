module Task3_2 where

{-
  Задание 3.2
  Структура данных "перевёрнутый список" представляет собой зеркально обращённый
  односвязный список из стандартной библиотеки, для которого взятие последнего
  элемента является наиболее эффективной операцией.
  Необходимо реализовать все функции и классы типов в данном файле.
-}

import Todo(todo)

data ReverseList a = RNil | RCons (ReverseList a) a

rlistToList :: ReverseList a -> [a]
rlistToList RNil = []
rlistToList (RCons list a) = a:(rlistToList list)

listToRList :: [a] -> ReverseList a
listToRList [] = RNil
listToRList (h:t) = RCons (listToRList t) h

-- Реализуйте классы Eq, Ord, Show, Monoid, Functor

instance Show a => Show (ReverseList a) where
    show RNil = "RNil"
    show (RCons list a) = "[ RCons " ++ (show list) ++ " " ++ (show a) ++ " ]"

instance Eq a => Eq (ReverseList a) where
    (==) RNil RNil = True
    (==) RNil _ = False
    (==) _ RNil = False
    (==) (RCons tailA a) (RCons tailB b)
        | a == b = tailA == tailB
        | otherwise = False

instance Ord a => Ord (ReverseList a) where
    compare RNil RNil = EQ
    compare RNil _ = LT
    compare _ RNil = GT
    compare (RCons tailA a) (RCons tailB b) = compare tailA tailB

instance Semigroup (ReverseList a) where
    (<>) x RNil = x
    (<>) x (RCons tail h) = RCons (x <> tail) h

instance Monoid (ReverseList a) where
    mempty = RNil

instance Functor ReverseList where
    fmap _ RNil = RNil
    fmap f (RCons tail h) = RCons (fmap f tail) (f h)