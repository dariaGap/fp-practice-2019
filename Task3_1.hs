module Task3_1 where

{-
  Задание 3.1
  Числа Пеано: представление чисел на основе нуля и операций "+1" и "-1".
  Необходимо реализовать все классы типов, характерные для целых чисел.
-}

data WeirdPeanoNumber = Zero | Succ WeirdPeanoNumber | Pred WeirdPeanoNumber

-- Реализуйте все классы типов, которым должны отвечать целые числа

peanoNorm :: WeirdPeanoNumber -> WeirdPeanoNumber
peanoNorm Zero = Zero
peanoNorm (Pred(Succ a)) = peanoNorm a
peanoNorm (Succ(Pred a)) = peanoNorm a
peanoNorm (Pred a) = Pred(peanoNorm a)
peanoNorm (Succ a) = Succ(peanoNorm a)


instance Show WeirdPeanoNumber where
    show Zero = "Zero"
    show (Succ a) = "Succ(" ++ (show a) ++ ")"
    show (Pred a) = "Pred(" ++ (show a) ++ ")"

instance Num WeirdPeanoNumber where
    (+) Zero b = b
    (+) a Zero = a
    (+) (Succ a) (Pred b) = a + b
    (+) (Pred a) (Succ b) = a + b
    (+) (Succ a) b = Succ (a + b)
    (+) (Pred a) b = Pred (a + b)
    (+) a b = b + a
    (*) Zero b = Zero
    (*) (Succ a) b = a*b + b
    (*) (Pred a) b = a*b - b
    (*) a b = b * a
    abs Zero = Zero
    abs (Succ a) = Succ(abs a)
    abs (Pred a) = Succ(abs a)
    signum Zero = Zero
    signum (Succ a) = Succ Zero
    signum (Pred a) = Pred Zero
    fromInteger 0 = Zero
    fromInteger x
        | x > 0 = Succ (fromInteger (x-1))
        | otherwise = Pred (fromInteger (x+1))
    negate Zero = Zero
    negate (Succ x) = Pred (negate x)
    negate (Pred x) = Succ (negate x)

instance Enum WeirdPeanoNumber where
    toEnum a = fromInteger (toInteger a)
    fromEnum a = fromIntegral( toInteger a)

instance Ord WeirdPeanoNumber where
    compare a b = case signum ((peanoNorm a) - (peanoNorm b)) of Zero -> EQ
                                                                 (Succ Zero) -> GT
                                                                 (Pred Zero) -> LT

instance Real WeirdPeanoNumber where
    toRational a = toRational (toInteger a)

instance Integral WeirdPeanoNumber where
    toInteger Zero = 0
    toInteger (Succ x) = toInteger x + 1
    toInteger (Pred x) = toInteger x - 1
    div a b = fromInteger (div (toInteger a) (toInteger b))
    mod a b = fromInteger (mod (toInteger a) (toInteger b))
    quotRem a b = case (a < Zero, b < Zero) of (True, True) -> (div (negate a) (negate b), mod (negate a) (negate b))
                                               (True, False) -> (negate(div (negate a) b), negate (mod (negate a) b))
                                               (False, True) -> (negate(div a (negate b)), negate(mod a (negate b)))
                                               (False, False) -> (div a b, mod a b)

instance Eq WeirdPeanoNumber where
    (==) Zero Zero = True
    (==) a b = case signum ((peanoNorm a) - (peanoNorm b)) of Zero -> True
                                                              otherwise -> False