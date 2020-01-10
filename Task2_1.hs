module Task2_1 where

{-
  Задание 2.1
  На основе структуры бинарного дерева из лекции необходимо реализовать свою структуру данных
  бинарного дерева поиска (без балансировки) и все операции, приведённые в данном файле
-}

import Todo(todo)
import Control.Exception
import Data.List


-- Ассоциативный массив на основе бинарного дерева поиска
-- Ключи - Integer, значения - произвольного типа
data TreeMap v = 
        EmptyTreeMap
        | Leaf Integer v
        | Node Integer v (TreeMap v) (TreeMap v)
    deriving (Show)

data NoSuchKeyException = NoSuchKeyException

instance Exception NoSuchKeyException where

instance Show NoSuchKeyException where
    show _ = "No such key"


-- Пустое дерево
emptyTree :: TreeMap v
emptyTree = EmptyTreeMap

-- Содержится ли заданный ключ в дереве?
contains :: TreeMap v -> Integer -> Bool
contains EmptyTreeMap _ = False
contains (Leaf x y) k 
    | k == x = True
    | otherwise = False
contains (Node x y l r) k
    | k == x = True
    | k < x = contains l k
    | otherwise = contains r k

-- Значение для заданного ключа
lookup :: Integer -> TreeMap v -> v
lookup _ EmptyTreeMap = throw NoSuchKeyException
lookup k (Leaf x y)  
    | k == x = y
    | otherwise = throw NoSuchKeyException
lookup k (Node x y l r) 
    | k == x = y
    | k < x = Task2_1.lookup k l 
    | otherwise = Task2_1.lookup k r

-- Вставка пары (ключ, значение) в дерево
insert :: (Integer, v) -> TreeMap v -> TreeMap v
insert (k, v) EmptyTreeMap = Leaf k v
insert (k, v) (Leaf x y) 
    | k < x = Node x y (Leaf k v) EmptyTreeMap
    | otherwise = Node x y EmptyTreeMap (Leaf k v)
insert (k, v) (Node x y l r) 
    | k < x = Node x y (Task2_1.insert (k, v) l) r
    | otherwise = Node x y l (Task2_1.insert (k, v) r)

-- Удаление элемента по ключу
-- Доп. функция insertNode - вставка части дерева
insertNode :: TreeMap v -> TreeMap v -> TreeMap v 
insertNode node EmptyTreeMap = node
insertNode EmptyTreeMap tree = tree
insertNode (Leaf k v) (Leaf x y) 
    | k < x = Node x y (Leaf k v) EmptyTreeMap
    | otherwise = Node x y EmptyTreeMap (Leaf k v)
insertNode (Node k v l r) (Leaf x y) 
    | k < x = Node x y (Node k v l r) EmptyTreeMap
    | otherwise = Node x y EmptyTreeMap (Node k v l r)
insertNode (Node k v l r) (Node x y l1 r1) 
    | k < x = Node x y (insertNode (Node k v l r) l1) r1
    | otherwise = Node x y l1 (insertNode (Node k v l r) r1)

remove :: Integer -> TreeMap v -> TreeMap v
remove _ EmptyTreeMap = throw NoSuchKeyException
remove i (Leaf x y) 
    | i == x = EmptyTreeMap
    | otherwise = throw NoSuchKeyException
remove i (Node x y l r)
    | x == i = insertNode l r
    | i < x = Node x y (remove i l) r
    | otherwise = Node x y l (remove i r)

-- Поиск ближайшего снизу ключа относительно заданного
-- Доп. функция findNearest - дополнительно хранит близжайший ключ
findNearest :: Integer -> (Integer, v) -> TreeMap v -> (Integer, v)
findNearest i (k, v) EmptyTreeMap = (k, v)
findNearest i (k, v) (Leaf x y)
    | (abs(i-k)) > (abs(x-k)) = (x, y)
    | otherwise = (k, v)
findNearest i (k, v) (Node x y l r)
    | (abs(i-k)) > (abs(x-k)) = findNearest i (findNearest i (x, y) l) r
    | otherwise = findNearest i (findNearest i (k, v) l) r

-- Доп. функция findNearestStart  - передает начальное значение ближайшего ключа
findNearestStart i (Node x y EmptyTreeMap EmptyTreeMap) = throw NoSuchKeyException
findNearestStart i (Node x y EmptyTreeMap (Leaf x1 y1)) = findNearest i (x1, y1) (Leaf x1 y1)
findNearestStart i (Node x y EmptyTreeMap (Node x1 y1 l1 r1)) = findNearest i (x1, y1) (Node x1 y1 l1 r1)
findNearestStart i (Node x y (Leaf x1 y1) r) = findNearest i (findNearest i (x1, y1) (Leaf x1 y1)) r
findNearestStart i (Node x y (Node x1 y1 l1 r1) r) = findNearest i (findNearest i (x1, y1) (Node x1 y1 l1 r1)) r

nearestLE :: Integer -> TreeMap v -> (Integer, v)
nearestLE _ EmptyTreeMap = throw NoSuchKeyException
nearestLE _ (Leaf x y) = throw NoSuchKeyException
nearestLE i (Node x y l r)
    | x == i = findNearestStart i (Node x y l r)
    | i < x = nearestLE i l
    | otherwise = nearestLE i r

-- Построение дерева из списка пар
-- Доп. функция makeTree - строит часть дерева из списка пар
makeTree :: [(Integer, v)] -> TreeMap v -> TreeMap v
makeTree [] tree = tree
makeTree (h:t) tree = makeTree t (Task2_1.insert h tree)

treeFromList :: [(Integer, v)] -> TreeMap v
treeFromList lst = makeTree lst emptyTree

-- Построение списка пар из дерева
-- Доп. функция lstFromTree - принимает дерево и список
lstFromTree :: TreeMap v -> [(Integer, v)] -> [(Integer, v)]
lstFromTree EmptyTreeMap lst = lst
lstFromTree (Leaf k v) lst = ((k, v): lst)
lstFromTree (Node k v l r) lst = lstFromTree r (lstFromTree l ((k, v) : lst))

listFromTree :: TreeMap v -> [(Integer, v)]
listFromTree t = lstFromTree t []
    

-- Поиск k-той порядковой статистики дерева
kMean :: Integer -> TreeMap v -> (Integer, v)

-- сравнение двух пар ключ - значение
comp (a,_) (b,_) 
    | a < b = LT
    | otherwise = GT 

kMean i t = (sortBy comp (listFromTree t)) !! (fromInteger i)
