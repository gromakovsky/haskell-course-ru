{-# LANGUAGE FlexibleInstances #-}
module EitherCPS where

import Control.Applicative
import Control.Monad

newtype Either' r a b = Either' {runEither' :: (a -> r) -> (b -> r) -> r}

caseEither' :: Either' r a b -> (a -> r) -> (b -> r) -> r
caseEither' x f g = runEither' x f g

-- конструкторы
left :: a -> Either' r a b
left a = Either' $ \f -> \g -> f a
right :: b -> Either' r a b
right b = Either' $ \f -> \g -> g b

-- инстансы
instance Functor (Either' r e) where
    fmap f ma = Either' $ \g -> \h -> caseEither' ma g (\a -> h (f a))

instance Applicative (Either' r e) where
    pure = right
    mf <*> ma = Either' $ \g -> \h -> caseEither' ma g (\a -> caseEither' mf g (\f -> h $ f a))

instance Monad (Either' r e) where
    return = right
    ma >>= f = Either' $ \g -> \h -> caseEither' ma g (\a -> runEither' (f a) g h)

