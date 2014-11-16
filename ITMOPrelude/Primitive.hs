{-# LANGUAGE NoImplicitPrelude #-}
module ITMOPrelude.Primitive where

import Prelude (Show, Read, error)

---------------------------------------------
-- Синтаксис лямбда-выражений

-- Эквивалентные определения
example1 x  = x
example1'   = \x -> x
example1''  = let y = \x -> x in y
example1''' = y where
    y = \x -> x

-- Снова эквивалентные определения
example2 x y  = x %+ y
example2' x   = \y -> x %+ y
example2''    = \x -> \y -> x %+ y
example2'''   = \x y -> x %+ y
example2''''  = let z = \x y -> x %+ y in z
example2''''' = z where
    z x = \y -> x %+ y

-- Зацикленное выражение
undefined = undefined

-- Ниже следует реализовать все термы, состоящие из undefined заглушки.
-- Любые термы можно переписывать (natEq и natLt --- хорошие кандидаты).

-------------------------------------------
-- Примитивные типы

-- Тип с единственным элементом
data Unit = Unit deriving (Show,Read)

-- Пара, произведение
data Pair a b = Pair { fst :: a, snd :: b } deriving (Show,Read)

-- Вариант, копроизведение
data Either a b = Left a | Right b deriving (Show,Read)

-- Частый частный случай, изоморфно Either Unit a
data Maybe a = Nothing | Just a deriving (Show,Read)

-- Частый частный случай, изоморфно Either Unit Unit
data Bool = False | True deriving (Show,Read)

-- Следует отметить, что встроенный if с этим Bool использовать нельзя,
-- зато case всегда работает.

-- Ну или можно реализовать свой if
if' True a b = a
if' False a b = b

-- Трихотомия. Замечательный тип, показывающий результат сравнения
data Tri = LT | EQ | GT deriving (Show,Read)

-------------------------------------------
-- Булевы значения

-- Логическое "НЕ"
not :: Bool -> Bool
not True = False
not False = True

infixr 3 &&
-- Логическое "И"
(&&) :: Bool -> Bool -> Bool
True  && x = x
False && _ = False

infixr 2 ||
-- Логическое "ИЛИ"
(||) :: Bool -> Bool -> Bool
True  || _ = True
False || x = x

-------------------------------------------
-- Натуральные числа

data Nat = Zero | Succ Nat deriving (Show,Read)

natZero = Zero     -- 0
natOne = Succ Zero -- 1
natTwo = Succ natOne
natThree = Succ natTwo
natFour = Succ natThree
natFive = Succ natFour
natSix = Succ natFive

-- -1
prev :: Nat -> Nat
prev Zero     = Zero
prev (Succ n) = n

-- Сравнивает два натуральных числа
natCmp :: Nat -> Nat -> Tri
natCmp Zero Zero = EQ
natCmp Zero (Succ _) = LT
natCmp (Succ _) Zero = GT
natCmp (Succ n) (Succ m) = natCmp n m

-- n совпадает с m 
natEq :: Nat -> Nat -> Bool
natEq a b = case cmp of EQ -> True
                        _  -> False
            where cmp = a `natCmp` b

-- n меньше m
natLt :: Nat -> Nat -> Bool
natLt a b = case cmp of LT -> True
                        _  -> False
            where cmp = a `natCmp` b

infixl 6 +.
-- Сложение для натуральных чисел
(+.) :: Nat -> Nat -> Nat
Zero     +. m = m
(Succ n) +. m = Succ (n +. m)

infixl 6 -.
-- Вычитание для натуральных чисел
(-.) :: Nat -> Nat -> Nat
Zero -. _ = Zero
n -. Zero = n
(Succ n) -. Succ(m) = n -. m

infixl 7 *.
-- Умножение для натуральных чисел
(*.) :: Nat -> Nat -> Nat
Zero     *. m = Zero
(Succ n) *. m = m +. (n *. m)

-- Целое и остаток от деления n на m
natDivMod :: Nat -> Nat -> Pair Nat Nat
natDivMod _ Zero = error "Division by zero"
natDivMod n m = let order = n `natCmp` m in
    case order of LT -> Pair Zero n
                  _  ->Pair div mod
                 where diff = n -. m
                       Pair prevDiv mod = diff `natDivMod` m
                       div = natOne +. prevDiv

natDiv n = fst . natDivMod n -- Целое
natMod n = snd . natDivMod n -- Остаток

-- Поиск GCD алгоритмом Евклида (должен занимать 2 (вычислителельная часть) + 1 (тип) строчки)
gcd :: Nat -> Nat -> Nat
gcd n Zero = n
gcd n m    = gcd m (n `natMod` m)

-------------------------------------------
-- Целые числа

-- Требуется, чтобы представление каждого числа было единственным
data Int = Pos Nat | Neg Nat deriving (Show,Read)

intZero   = Pos natZero -- 0
intOne    = Pos natOne  -- 1
intNegOne = Neg natZero -- -1

-- n -> - n
intNeg :: Int -> Int
intNeg (Pos Zero) = intZero
intNeg (Neg n) = Pos (Succ n)
intNeg (Pos n) = Neg (prev n)

-- Дальше также как для натуральных
intCmp :: Int -> Int -> Tri
intCmp (Pos _) (Neg _) = GT
intCmp (Neg _) (Pos _) = LT
intCmp (Pos a) (Pos b) = a `natCmp` b
intCmp (Neg a) (Neg b) = b `natCmp` a

intEq :: Int -> Int -> Bool
intEq a b = case order of EQ -> True
                          _ -> False
                          where order = intCmp a b

intLt :: Int -> Int -> Bool
intLt a b = case order of LT -> True
                          _ -> False
                          where order = intCmp a b

infixl 6 .+., .-.
-- У меня это единственный страшный терм во всём файле
(.+.) :: Int -> Int -> Int
(Pos a) .+. (Pos b) = Pos (a +. b)
(Pos a) .+. (Neg b) = case order of LT -> Neg $ prev (b -. a)
                                    EQ -> intNegOne
                                    GT -> Pos $ prev (a -. b)
                                    where order = natCmp a b
a@(Neg _) .+. b@(Pos _) = b .+. a
a .+. b = intNeg $ intNeg a .+. intNeg b

(.-.) :: Int -> Int -> Int
n .-. m = n .+. (intNeg m)

infixl 7 .*.
(.*.) :: Int -> Int -> Int
Pos a .*. Pos b = Pos $ a *. b
a@(Neg _) .*. b@(Pos _) = intNeg $ intNeg a .*. b
a@(Pos _) .*. b@(Neg _) = intNeg $ a .*. intNeg b
n .*. m = intNeg n .*. intNeg m

asNat :: Int -> Nat
asNat (Neg _) = error "Negative numbers can't be Nat"
asNat (Pos a) = a

-- Модуль
intAbs :: Int -> Nat
intAbs (Pos a) = a
intAbs n = asNat $ intNeg n

-------------------------------------------
-- Рациональные числа

data Rat = Rat Int Nat deriving (Show,Read)

ratZero = Rat intZero natSix
ratZero' = Rat intZero natFive
ratHalf = Rat intOne natTwo
ratNegHalf = Rat intNegOne natTwo
ratOne = Rat intOne natOne
ratNegOne = Rat intNegOne natOne

ratNeg :: Rat -> Rat
ratNeg (Rat x y) = Rat (intNeg x) y

-- У рациональных ещё есть обратные элементы
ratInv :: Rat -> Rat
ratInv (Rat (Pos Zero) _) = error "Zero does not have inverse"
ratInv (Rat (Pos n) m) = Rat (Pos m) n
ratInv (Rat n m) = ratNeg . ratInv $ Rat (intNeg n) m

-- Дальше как обычно
ratCmp :: Rat -> Rat -> Tri
ratCmp (Rat a b) (Rat c d) = intCmp (a .*. Pos d) (c .*. Pos b)

ratEq :: Rat -> Rat -> Bool
ratEq a b = case order of EQ -> True
                          _ -> False
                          where order = ratCmp a b

ratLt :: Rat -> Rat -> Bool
ratLt a b = case order of LT -> True
                          _ -> False
                          where order = ratCmp a b

infixl 7 %+, %-
(%+) :: Rat -> Rat -> Rat
Rat a b %+ Rat c d = Rat (a .*. Pos d .+. c .*. Pos b) (b *. d)

(%-) :: Rat -> Rat -> Rat
n %- m = n %+ (ratNeg m)

infixl 7 %*, %/
(%*) :: Rat -> Rat -> Rat
Rat a b %* Rat c d = Rat (a .*. c) (b *. d)

(%/) :: Rat -> Rat -> Rat
n %/ m = n %* (ratInv m)

-------------------------------------------
-- Операции над функциями.
-- Определены здесь, но использовать можно и выше

infixr 9 .
f . g = \ x -> f (g x)

infixr 0 $
f $ x = f x

-- Эквивалентные определения
example3   a b c = gcd a (gcd b c)
example3'  a b c = gcd a $ gcd b c
example3'' a b c = ($) (gcd a) (gcd b c)

-- И ещё эквивалентные определения
example4  a b x = (gcd a (gcd b x))
example4' a b = gcd a . gcd b

