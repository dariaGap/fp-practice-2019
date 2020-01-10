module Task4_1 where

{-
  Задание 4.1
  Реализация монады над функцией.
  Реализуйте все требуемые классы типов и объясните, почему они реализованы именно так.
-}

-- Монада над функцией. В качестве входного значения `fun` может быть что угодно
-- Собственно, почему бы не `String`?
data FunMonad a = FunMonad { fun :: String -> a }

-- реализуйте классы `Functor`, `Applicative` и `Monad` для типа `FunMonad`

instance Functor FunMonad where
-- Применение функции f к FunMonad
    fmap f (FunMonad x) = FunMonad (f . x)

instance Applicative FunMonad where
-- pure x возвращает FunMonad, дающую x при любом аргументе
    pure x = FunMonad (\_ -> x)
-- <*> извлекает функцию f типа (String -> a -> b) из FunMonad и пременяет ее к FunMonad x
    (<*>) (FunMonad f) (FunMonad x) = FunMonad (\a -> f a (x a))

instance Monad FunMonad where
-- >>= применяет функцию f к FunMonad и возвращает результат, обернутый в FunMonad
    (>>=) (FunMonad x) f = FunMonad (\a -> fun(f (x a)) a)