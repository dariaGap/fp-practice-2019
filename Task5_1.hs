module Task5_1 where

import Todo(todo)
import Control.Exception

-- Структура двусвязного списка из лекции про ленивость

data DList a = DNil 
             | DCons { 
                left :: (DList a), 
                current :: a, 
                right :: (DList a) 
             }

instance (Show a) => Show (DList a) where
    show it = "[" ++ showBody it ++ "]"
              where showBody DNil = ""
                    showBody (DCons _ h DNil) = show h
                    showBody (DCons _ h t) = show h ++ ", " ++ showBody t

instance (Eq a) => Eq (DList a) where
    DNil == DNil = True
    (DCons _ h1 t1) == (DCons _ h2 t2) = h1 == h2 && t1 == t2
    _ == _ = False

list2dlist :: [a] -> DList a
list2dlist lst = list2dlist' DNil lst

list2dlist' :: DList a -> [a] -> DList a
list2dlist' _ [] = DNil
list2dlist' left (h: t) = 
    let rec = DCons left h (list2dlist' rec t)
    in rec


data NoSuchIndexException = NoSuchIndexException

instance Exception NoSuchIndexException where

instance Show NoSuchIndexException where
    show _ = "No such index"

-- Реализуйте функции индексирования, вставки и удаления элементов
index :: DList a -> Int -> a
index DNil _ = throw NoSuchIndexException
index (DCons l h r) 0 = h
-- Возможность двунаправленного поиска по индексу
index (DCons l h r) n
    | n > 0 = index r (n - 1)
    | otherwise = index l (n + 1)

-- Реализована возможность двунаправленной вставки элементов
-- Так как распечатка списка начинается с головы (индекс 0), то
-- для вывода элементов с индексами меньше нуля необходимо воспользоваться поиском по индексу
insertAt :: DList a -> Int -> a -> DList a
insertAt (DCons l h r) 0 value =
    let rec = DCons l value (DCons rec h r)
    in rec
insertAt (DCons l h r) 1 value =
    let rec = DCons l h (DCons rec value r)
    in rec
insertAt (DCons l h r) (-1) value =
    let rec = DCons (DCons l value rec) h r
    in rec
insertAt (DCons l h DNil) n value
    | n > 0 = insertAt (DCons l h DNil) 1 value
    | otherwise = DCons (insertAt l (n+1) value) h DNil
insertAt (DCons DNil h r) n value
    | n < 0 = insertAt (DCons DNil h r) (-1) value
    | otherwise = DCons DNil h (insertAt r (n-1) value)
insertAt (DCons l h r) n value
    | n > 0 = DCons l h (insertAt r (n-1) value)
    | otherwise = DCons (insertAt l (n+1) value) h r

-- Реализована возможность двунаправленного удаления элементов
-- Так как распечатка списка начинается с головы (индекс 0), то
-- для вывода элементов с индексами меньше нуля необходимо воспользоваться поиском по индексу
removeAt :: DList a -> Int -> DList a
removeAt DNil _ = throw NoSuchIndexException
removeAt (DCons DNil h DNil) 0 = DNil
removeAt (DCons (DCons ll lh lr) h DNil) 0 = DCons ll lh DNil
removeAt (DCons l h (DCons rl rh rr)) 0 = DCons l rh rr
removeAt (DCons l h (DCons rl rh rr)) 1 =  DCons l h rr
removeAt (DCons (DCons ll lh lr) h r) (-1) =  DCons ll h r
removeAt (DCons l h r) n
    | n > 0 = DCons l h (removeAt r (n - 1))
    | otherwise = DCons (removeAt l (n + 1)) h r