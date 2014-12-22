{-# LANGUAGE FlexibleInstances, Rank2Types #-}
module EitherCPS where

import Control.Applicative
import Control.Monad

newtype Either' a b = Either' {runEither' :: forall r . (a -> r) -> (b -> r) -> r}

caseEither' :: Either' a b -> (a -> r) -> (b -> r) -> r
caseEither' x f g = runEither' x f g

-- конструкторы
left :: a -> Either' a b
left a = Either' $ \f -> \g -> f a
right :: b -> Either' a b
right b = Either' $ \f -> \g -> g b

-- инстансы
instance Functor (Either' r) where
    fmap f ma = Either' $ \g -> \h -> caseEither' ma g (\a -> h (f a))

instance Applicative (Either' r) where
    pure = right
    mf <*> ma = Either' $ \g -> \h -> caseEither' ma g (\a -> caseEither' mf g (\f -> h $ f a))

instance Monad (Either' r) where
    return = right
    ma >>= f = Either' $ \g -> \h -> caseEither' ma g (\a -> runEither' (f a) g h)

