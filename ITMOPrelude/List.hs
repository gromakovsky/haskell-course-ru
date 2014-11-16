{-# LANGUAGE NoImplicitPrelude #-}
module ITMOPrelude.List where

import Prelude (Show,Read,error,String,show)
import ITMOPrelude.Primitive

---------------------------------------------
-- Что надо делать?
--
-- Все undefined превратить в требуемые термы.
-- Звёздочкой (*) отмечены места, в которых может потребоваться думать.

---------------------------------------------
-- Определение

data List a = Nil |  Cons a (List a) deriving (Show,Read)

testList = Cons natOne $ Cons natTwo $ Cons natFour $ Cons natZero Nil
---------------------------------------------
-- Операции

-- Длина списка
length :: List a -> Nat
length Nil         = natZero
length (Cons _ xs) = Succ $ length xs

-- Склеить два списка за O(length a)
(++) :: List a -> List a -> List a
Nil       ++ xs = xs
Cons x xs ++ ys = Cons x (xs ++ ys)

-- Список без первого элемента
tail :: List a -> List a
tail Nil         = error "tail can't be applied to empty list"
tail (Cons _ xs) = xs

-- Список без последнего элемента
init :: List a -> List a
init Nil          = error "init can't be applied to empty list"
init (Cons x Nil) = Nil
init (Cons x xs)  = Cons x (init xs)

-- Первый элемент
head :: List a -> a
head Nil        = error "head can't be applied to empty list"
head (Cons x _) = x

-- Последний элемент
last :: List a -> a
last Nil          = error "last can't be applied to empty list"
last (Cons x Nil) = x
last (Cons x xs)  = last xs

-- n первых элементов списка
take :: Nat -> List a -> List a
take Zero _               = Nil
take _ Nil = error "can't take more elements from list than it contains"
take (Succ n) (Cons x xs) = Cons x (take n xs)

-- Список без n первых элементов
drop :: Nat -> List a -> List a
drop Zero x               = x
drop _ Nil = error "can't drop more elements from list than it contains"
drop (Succ n) (Cons _ xs) = drop n xs

-- Оставить в списке только элементы удовлетворяющие p
filter :: (a -> Bool) -> List a -> List a
filter p Nil         = Nil
filter p (Cons x xs) = if' (p x) (Cons x (filter p xs)) (filter p xs)

-- Функция, чтобы потестить gFilter
testFunc :: (Nat -> Maybe String)
testFunc n = if' (n `natLt` natThree) (Just $ show n) Nothing

-- Обобщённая версия. Вместо "выбросить/оставить" p
-- говорит "выбросить/оставить b".
gfilter :: (a -> Maybe b) -> List a -> List b
gfilter p Nil = Nil
gfilter p (Cons a l) = case p a of Nothing -> gfilter p l
                                   Just b -> Cons b (gfilter p l)

-- Копировать из списка в результат до первого нарушения предиката
-- takeWhile (< 3) [1,2,3,4,1,2,3,4] == [1,2]
takeWhile :: (a -> Bool) -> List a -> List a
takeWhile p Nil         = Nil
takeWhile p (Cons x xs) = if' (p x) (Cons x $ takeWhile p xs) Nil

-- Не копировать из списка в результат до первого нарушения предиката,
-- после чего скопировать все элементы, включая первый нарушивший
-- dropWhile (< 3) [1,2,3,4,1,2,3,4] == [3,4,1,2,3,4]
dropWhile :: (a -> Bool) -> List a -> List a
dropWhile p Nil           = Nil
dropWhile p l@(Cons x xs) = if' (p x) (dropWhile p xs) l

-- Разбить список по предикату на (takeWhile p xs, dropWhile p xs),
-- но эффективнее
span :: (a -> Bool) -> List a -> Pair (List a) (List a)
span p Nil           = Pair {fst = Nil, snd = Nil}
span p l@(Cons x xs) = if' (p x) Pair {fst = Cons x a, snd = b}
                                 Pair {fst = Nil, snd = l}
                       where Pair a b = span p xs

-- Разбить список по предикату на (takeWhile (not . p) xs, dropWhile (not . p) xs),
-- но эффективнее
break :: (a -> Bool) -> List a -> Pair (List a) (List a)
break p = span $ not . p

-- n-ый элемент списка (считая с нуля)
(!!) :: List a -> Nat -> a
Nil !! n = error "!!: empty list"
Cons x _  !! Zero   = x
Cons _ xs !! Succ n = xs !! n

-- Добавить в конец
append :: List a -> a -> List a
append xs x = xs ++ (Cons x Nil)

-- Список задом на перёд
reverse :: List a -> List a
reverse Nil         = Nil
reverse (Cons x xs) = append (reverse xs) x

-- (*) Все подсписки данного списка
subsequences :: List a -> List (List a)
subsequences Nil         = Cons Nil Nil
subsequences (Cons x xs) = subsequences xs ++ map (Cons x) (subsequences xs)

-- (*) Все перестановки элементов данного списка

-- Вспомогательные функции
-- Вставить в список элемент на заданную позицию
insertInto :: List a -> a -> Nat -> List a
insertInto Nil val _                = Cons val Nil
insertInto l val Zero               = Cons val l
insertInto (Cons x xs) val (Succ n) = Cons x $ insertInto xs val n

-- Вставить во все списки элемент на заданную позицию
insertIntoAll :: List (List a) -> a -> Nat -> List (List a)
insertIntoAll lists val n = map (\list -> insertInto list val n) lists

-- natList n = [n - 1 .. 0]
natList :: Nat -> List Nat
natList Zero     = Nil
natList (Succ n) = Cons n $ natList n

permutations :: List a -> List (List a)
permutations Nil           = Cons Nil Nil
permutations l@(Cons x xs) = let indexes = natList $ length l; perms = permutations xs in 
                                concatMap (insertIntoAll perms x) indexes

-- (*) Если можете. Все перестановки элементов данного списка
-- другим способом
permutations' :: List a -> List (List a)
permutations' = undefined

-- Повторяет элемент бесконечное число раз
repeat :: a -> List a
repeat x = Cons x $ repeat x

-- Левая свёртка
-- порождает такое дерево вычислений:
--         f
--        / \
--       f   ...
--      / \
--     f   l!!2
--    / \
--   f   l!!1
--  / \
-- z  l!!0
foldl :: (a -> b -> a) -> a -> List b -> a
foldl f z Nil = z
foldl f z xs  = f (foldl f z $ init xs) (last xs)

-- Тот же foldl, но в списке оказываются все промежуточные результаты
-- last (scanl f z xs) == foldl f z xs
scanl :: (a -> b -> a) -> a -> List b -> List a
scanl f z Nil = Cons z Nil
scanl f z xs  = previous `append` f (last previous) (last xs)
                         where previous = scanl f z $ init xs

-- Правая свёртка
-- порождает такое дерево вычислений:
--    f
--   /  \
-- l!!0  f
--     /  \
--   l!!1  f
--       /  \
--    l!!2  ...
--           \
--            z
--            
foldr :: (a -> b -> b) -> b -> List a -> b
foldr f z Nil         = z
foldr f z (Cons x xs) = f x $ foldr f z xs

-- Аналогично
--  head (scanr f z xs) == foldr f z xs.
scanr :: (a -> b -> b) -> b -> List a -> List b
scanr f z Nil         = Cons z Nil
scanr f z (Cons x xs) = Cons (f x $ head previous) previous
                      where previous = scanr f z xs

-- Должно завершаться за конечное время
finiteTimeTest = take (Succ $ Succ $ Succ $ Succ Zero) $ foldr (Cons) Nil $ repeat Zero

-- Применяет f к каждому элементу списка
map :: (a -> b) -> List a -> List b
map f Nil         = Nil
map f (Cons x xs) = Cons (f x) (map f xs)

-- Склеивает список списков в список
concat :: List (List a) -> List a
concat = foldr (++) Nil

-- Эквивалент (concat . map), но эффективнее
concatMap :: (a -> List b) -> List a -> List b
concatMap f Nil         = Nil
concatMap f (Cons x xs) = f x ++ concatMap f xs

-- Сплющить два списка в список пар длинны min (length a, length b)
zip :: List a -> List b -> List (Pair a b)
zip _ Nil                   = Nil
zip Nil _                   = Nil
zip (Cons x xs) (Cons y ys) = Cons Pair {fst = x, snd = y} $ zip xs ys

-- Аналогично, но плющить при помощи функции, а не конструктором Pair
zipWith :: (a -> b -> c) -> List a -> List b -> List c
zipWith f _ Nil                   = Nil
zipWith f Nil _                   = Nil
zipWith f (Cons x xs) (Cons y ys) = Cons (f x y) $ zipWith f xs ys

