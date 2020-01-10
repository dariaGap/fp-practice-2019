module Task2_2 where

{-
  Задание 2.2
  Необходимо реализовать функции foldl, foldr и unfoldr, а также все остальные функции
  в данном файле _на основе этих трёх_
-}

import Todo(todo)

import Prelude hiding (foldl, foldr, unfoldr, map, concatMap,
    filter, maxBy, minBy, reverse, sum, product, elem)

foldl :: (b -> a -> b) -> b -> [a] -> b
foldl _ x0 [] = x0
foldl f x0 (h:t) = foldl f (f x0 h) t

foldr :: (a -> b -> b) -> b -> [a] -> b
foldr _ x0 [] = x0
foldr f x0 (h:t) = f h (foldr f x0 t)

unfoldr :: (b -> Maybe (a, b)) -> b -> [a]
unfoldr f b = case f b of
    Nothing -> []
    Just(a,b) -> (a : (unfoldr f b))

-- Сумма всех элементов списка (пример)
sum :: [Integer] -> Integer
sum lst = foldl (+) 0 lst

-- Переворот списка (Пример)
reverse :: [a] -> [a]
reverse lst = foldl f [] lst where f t h = h:t

-- Отображение элементов списка
map :: (a -> b) -> [a] -> [b]
map f lst = unfoldr (funk f) lst

-- дополнительная функция, применяющая f к первому элементу списка
funk _ [] = Nothing
funk f (h:t) = Just (f h, t)

-- Произведение всех элементов списка
product :: [Integer] -> Integer
product lst = foldl (*) 1 lst

-- Выделение из списка Maybe всех существующих значений
catMaybes :: [Maybe a] -> [a]
catMaybes lst = unfoldr f1 lst

-- дополнительная функция, пропускает элемент списка, если он Nothing
f1 [] = Nothing
f1 (h:t) = case h of 
    Nothing -> f1 t
    Just x -> Just (x, t)

-- Диагональ матрицы
diagonal :: [[a]] -> [a]
diagonal lst = unfoldr (f2 lst) 0

-- дополнительная функция, выбирает i-ый элемент из i-ой строки (предполагается квадратная матрица)
f2 lst i 
    | i < length lst = Just (((lst !! i)) !! i, i+1)
    | otherwise = Nothing


-- Фильтр для всех элементов, не соответствующих предикату
filterNot :: (a -> Bool) -> [a] -> [a]
filterNot f lst = unfoldr (f3 f) lst

-- дополнительная функция, пропускает элемент списка, если f от этого элемента возвращает True
f3 _ [] = Nothing
f3 f (h:t)
    | f h == True = f3 f t
    | otherwise = Just(h, t)

-- Поиск элемента в списке
elem :: (Eq a) => a -> [a] -> Bool
elem el lst = foldl (||) False (map (== el) lst)

-- Список чисел в диапазоне [from, to) с шагом step
rangeTo :: Integer -> Integer -> Integer -> [Integer]
rangeTo from to step = unfoldr (f4 step to) from

-- дополнительная функция, вычисляет следующее значение в списке
f4 step to i 
    | i > to = Nothing
    | otherwise = Just (i, i + step)

-- Конкатенация двух списков
append :: [a] -> [a] -> [a]
append lst1 lst2 = unfoldr f5 [lst1, lst2]

f5 [[],[]] = Nothing
f5 [(h:[]), lst2] = Just(h,[lst2,[]])
f5 [[], (h:t)] = Just(h,[t,[]])
f5 [(h:t), lst2]= Just(h,[t,lst2])

-- Разбиение списка lst на куски размером n
-- (последний кусок может быть меньше)
groups :: [a] -> Integer -> [[a]]
groups lst n = unfoldr (f6 (fromIntegral n)) lst

f6 n [] = Nothing
f6 0 _ = Nothing
f6 n lst = Just(splitAt n lst)