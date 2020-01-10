module Task6 where

{-
В этом файле приведён код, написанный (совместными усилиями) на лекции

Модифицируйте представленный тут парсер таким образом, чтобы он поддерживал унарные операции 
(отрицание и факториал), а также числа с плавающей точкой
-}

import Text.Parsec hiding(digit)
import Data.Functor
import Control.Exception


data IncorrectFactorialException = IncorrectFactorialException

instance Exception IncorrectFactorialException where

instance Show IncorrectFactorialException where
    show _ = "Incorrect factorial argument"

type Parser a = Parsec String () a

digit :: Parser Char
digit = oneOf ['0'..'9']

number :: Parser Integer
number = read <$> many1 digit

dnum :: Parser String
dnum = do
    char '.'
    t <- many1 digit
    return ('.' : t)

dnumber :: Parser Double
dnumber = do
    lhv <- many1 digit
    rhv <- option "" dnum
    return (read (lhv ++ rhv))

applyMany :: a -> [a -> a] -> a
applyMany x [] = x
applyMany x (h:t) = applyMany (h x) t

div_ :: Parser (Double -> Double -> Double)
div_ = do
    char '/'
    return (/)

star :: Parser (Double -> Double -> Double)
star = do
    char '*'
    return (*)

plus :: Parser (Double -> Double -> Double)
plus = do
    char '+'
    return (+)

minus :: Parser (Double -> Double -> Double)
minus = do
    char '-'
    return (-)

multiplication :: Parser Double
multiplication = do
    spaces
    lhv <- atom
    spaces
    t <- many tail
    return $ applyMany lhv t
    where tail = fact <|>
            do
                f <- star <|> div_
                spaces
                rhv <- atom
                spaces
                return (`f` rhv)

addition :: Parser Double
addition = do
    spaces
    lhv <- multiplication
    spaces
    t <- many tail
    return $ applyMany lhv t
    where tail = 
            do
                f <- plus <|> minus
                spaces
                rhv <- multiplication
                spaces
                return (`f` rhv)

neg :: Parser Double
neg = do
    char '-'
    rhv <- atom
    return ((-1) * rhv)

fact :: Parser (Double -> Double)
fact = do
    char '!'
    return factorial

factorial :: Double -> Double
factorial n
    | (n < 0) = throw IncorrectFactorialException
    | ((snd (properFraction n)) /= 0) = throw IncorrectFactorialException
    | otherwise = product [n, n-1 .. 1]

atom :: Parser Double
atom = dnumber <|> neg <|> do
    char '('
    res <- addition
    char ')'
    return res