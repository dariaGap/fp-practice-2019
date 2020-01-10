module Task1_2 where

{-
  Задание 1.2
  Необходимо реализовать четыре любые функции в данном файле
-}

import Todo(todo)

-- синус числа (формула Тейлора)
sin :: Double -> Double
sin x = todo

-- косинус числа (формула Тейлора)
cos :: Double -> Double
cos x = todo

-- наибольший общий делитель двух чисел
gcd :: Integer -> Integer -> Integer
gcd x y = todo

-- существует ли полный целочисленный квадрат в диапазоне [from, to)?
doesSquareBetweenExist :: Integer -> Integer -> Bool
doesSquareBetweenExist from to 
    | ceiling (sqrt $ fromIntegral from) * ceiling (sqrt $ fromIntegral from) < to = True
    | otherwise = False

-- является ли дата корректной с учётом количества дней в месяце и
-- вискокосных годов?
isDateCorrect :: Integer -> Integer -> Integer -> Bool
isDateCorrect day month year 
    | (month > 12) || (month < 1) || (day < 1) || (year < 0) = False
    | (month `elem` [1,3,5,7,8,10,12]) && (day <= 31) = True
    | (month `elem` [4,6,9,11]) && (day <= 30) = True
    | (month == 2) && (day <= 29) && (year `mod` 4 == 0)= True
    | (month == 2) && (day <= 28) = True
    | otherwise = False

-- возведение числа в степень, duh
-- готовые функции и плавающую арифметику использовать нельзя
pow :: Integer -> Integer -> Integer
pow_my :: Integer -> Integer -> Integer -> Integer
pow_my _ 0 _ = 1
pow_my x 1 _ = x
pow_my x y k = pow_my (x*k) (y-1) k
pow x y = pow_my x y x

-- является ли данное число простым?
isPrime :: Integer -> Bool
isPrime_my :: Integer -> Integer -> Bool
isPrime_my n 1 = True
isPrime_my n k = if n `mod` k == 0 then False
    else isPrime_my n (k - 1)
isPrime 1 = False
isPrime x = isPrime_my x (x - 1)

type Point2D = (Double, Double)

-- рассчитайте площадь многоугольника по формуле Гаусса
-- многоугольник задан списком координат
shapeArea :: [Point2D] -> Double
shapeArea points = todo

-- треугольник задан своими координатами.
-- функция должна вернуть
--  0, если он тупоугольный
--  1, если он остроугольный
--  2, если он прямоугольный
--  -1, если это не треугольник
triangleKind :: Point2D -> Point2D -> Point2D -> Integer
triangleKind a b c = todo
