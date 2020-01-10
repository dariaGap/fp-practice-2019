module Task1_1 where

{-
  Задание 1.1
  Необходимо реализовать все операции, описанные в данном файле
-}

import Todo(todo)

data Term = IntConstant{ intValue :: Int }           -- числовая константа
            | Variable{ varName :: String }          -- переменная
            | BinaryTerm{ lhv :: Term, rhv :: Term, op :: String } -- бинарная операция
            deriving(Show,Eq)

-- Для бинарных операций необходима не только реализация, но и адекватные
-- ассоциативность и приоритет
(|+|) :: Term -> Term -> Term
(|+|) (IntConstant l) (IntConstant r) = IntConstant (l + r)
(|+|) l r = BinaryTerm l r "|+|"

(|-|) :: Term -> Term -> Term
(|-|) (IntConstant l) (IntConstant r) = IntConstant (l - r)
(|-|) l r = BinaryTerm l r "|-|"

(|*|) :: Term -> Term -> Term
(|*|) (IntConstant l) (IntConstant r) = IntConstant (l * r)
(|*|) l r = BinaryTerm l r "|*|"

infixl 7 |*|
infixl 6 |+|, |-|


-- Заменить переменную `varName` на `replacement`
-- во всём выражении `expression`
replaceVar :: String -> Term -> Term -> Term
replaceVar varName replacement (Variable expression)
    | varName == expression = replacement
replaceVar varName replacement (BinaryTerm l r op) = BinaryTerm (replaceVar varName replacement l) (replaceVar varName replacement r) op
replaceVar _ _ expression = expression

-- Посчитать значение выражения `Term`
-- если оно состоит только из констант
evaluate :: Term -> Term
evaluate (BinaryTerm l r "|+|") = (evaluate l) |+| (evaluate r)
evaluate (BinaryTerm l r "|-|") = (evaluate l) |-| (evaluate r)
evaluate (BinaryTerm l r "|*|") = (evaluate l) |*| (evaluate r)
evaluate expression = expression


