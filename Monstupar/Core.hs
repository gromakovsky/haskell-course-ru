-- Extremely simple but monstrously stupid (and slow) monadic parser
-- combinator library
module Monstupar.Core
    ( ParseError(..)
    , Monstupar, runMonstupar
    , ok, isnot, eof, (<|>), like
    ) where

--------------------------------------------------------------------------------
-- Определения

-- Тело этого определения можно заменить на всё, что захочется
data ParseError = ParseError String | ShouldNotParse | EOFExpected
                deriving (Show) -- лишь бы show был

newtype Monstupar s a = Monstupar { runMonstupar :: [s] -> Either ParseError ([s], a) }

instance Monad (Monstupar s) where
    return a = Monstupar $ \xs -> Right (xs, a)
    ma >>= f = Monstupar $ \xs -> runMonstupar ma xs >>= (\(xs, a) -> runMonstupar (f a) xs)

--------------------------------------------------------------------------------
-- Примитивные парсеры.
-- Имена и сигнатуры функций менять нельзя, тела можно

-- Всё хорошо
ok :: Monstupar s ()
ok = return ()
--ok = Monstupar $ \s -> Right (s , ())

-- Не должно парситься парсером p
isnot :: Monstupar s () -> Monstupar s ()
isnot p = Monstupar $ \xs -> case runMonstupar p xs of (Left _) -> Right (xs, ())
                                                       (Right _) -> Left ShouldNotParse

-- Конец ввода
eof :: Monstupar s ()
eof = Monstupar next where
    next [] = Right ([], ())
    next _  = Left EOFExpected

infixr 2 <|>
-- Сначала первый парсер, если он фейлится, то второй
(<|>) :: Monstupar s a -> Monstupar s a -> Monstupar s a
p1 <|> p2 = Monstupar $ \xs -> case runMonstupar p1 xs of res@(Right _) -> res
                                                          (Left _) -> runMonstupar p2 xs


-- В голове ввода сейчас нечто, удовлетворяющее p
like :: (s -> Bool) -> Monstupar s s
like p = Monstupar next where
    next [] = Left $ ParseError "Expected something like something but EOF found"
    next (x:xs) | p x = Right (xs, x)
                | otherwise = Left $ ParseError "Like didn't match"

-- Сюда можно добавлять ещё какие-то примитивные парсеры
-- если они понадобятся

