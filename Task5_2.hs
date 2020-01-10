module Task5_2 where

import Todo(todo)
import Control.Exception

data IncorrectIndexException = IncorrectIndexException

instance Exception IncorrectIndexException where

instance Show IncorrectIndexException where
    show _ = "Incorrect index"

-- Зиппер из лекции 

data Zipper a = Zipper [a] [a]

-- Реализуйте экземпляры классов Show и Eq для этого типа
instance Show a => Show (Zipper a) where
    show (Zipper l r) = (show l) ++ " " ++ (show r)

instance Eq a => Eq (Zipper a) where
    (==) a b = (toList a) == (toList b)

fromList :: [a] -> Zipper a
fromList lst = Zipper [] lst

toList :: Zipper a -> [a]
toList (Zipper [] r) = r
toList x = toList (goLeft x)

goRight :: Zipper a -> Zipper a
goRight z@(Zipper _ []) = z
goRight (Zipper l (rh:rt)) = Zipper (rh:l) rt

goLeft :: Zipper a -> Zipper a
goLeft z@(Zipper [] _) = z
goLeft (Zipper (lh:lt) r) = Zipper lt (lh:r)

putRight :: a -> Zipper a -> Zipper a
putRight x (Zipper l r) = Zipper l (x:r)

putLeft :: a -> Zipper a -> Zipper a
putLeft x (Zipper l r) = Zipper (x:l) r

removeRight :: Zipper a -> Zipper a
removeRight (Zipper l (_:rt)) = Zipper l rt

removeLeft :: Zipper a -> Zipper a
removeLeft (Zipper (_:lt) r) = Zipper lt r

-- Используя приведённые выше функции, реализуйте функцию конкатенации
-- вставки подсписка в середину и выделения подсписка

concat :: Zipper a -> Zipper a -> Zipper a
concat (Zipper l []) (Zipper [] r) = Zipper l r
concat a b = Task5_2.concat (goRight a) (goLeft b)

insertManyAt :: Int -> Zipper a -> Zipper a -> Zipper a
insertManyAt ind what into = insert (maxRight what) (index ind (maxRight into))

-- Вспомогательные функции для insertManyAt

-- Формирует Zipper вида (Zipper [] r)
maxRight :: Zipper a -> Zipper a
maxRight z@(Zipper [] _) = z
maxRight z = maxRight (goLeft z)

-- Перемещает указатель таким образом, что правый список начинается с элемента по зданному индексу
-- Ограничение - необходимо перед началом поиска сдвинуть все элементы зиппера в правую часть
index :: Int -> Zipper a -> Zipper a
index 0 x = x
index n x = index (n - 1) (goRight x)

-- Вставляет элементы зиппера a в зиппер b по указателю, разделяющему списки в зиппере b
-- Ограничение - необходимо перед началом поиска сдвинуть все элементы вставляемого зиппера в правую часть
insert :: Zipper a -> Zipper a -> Zipper a
insert (Zipper [] []) into = into 
insert (Zipper [] (h:t)) into = insert (Zipper [] t) (putLeft h into)


subZipper :: Int -> Int -> Zipper a -> Zipper a
subZipper from to input
    | to >= from = cutRight (moveTo (to - from)(cutFor from (maxRight input)))
    | otherwise = throw IncorrectIndexException

-- Вспомогательные функции для subZipper

-- Обрезает n первых элементов зиппера
-- Ограничение - необходимо перед вызовом функции сдвинуть все элементы зиппера в правую часть
cutFor :: Int -> Zipper a -> Zipper a
cutFor _ z@(Zipper [] []) = z
cutFor 0 z = z
cutFor n z = cutFor (n - 1)(removeRight z)

-- Удаляет всесь правый список зиппера
cutRight :: Zipper a -> Zipper a
cutRight z@(Zipper _ []) = z
cutRight z = cutRight (removeRight z)

-- Перемещает n элементов зиппера из правого списка в левый
moveTo :: Int -> Zipper a -> Zipper a
moveTo 0 z = goRight z
moveTo n z = moveTo (n - 1) (goRight z)