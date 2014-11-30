{-# LANGUAGE FlexibleInstances #-}
module IdentityCPS where

import Control.Applicative
import Control.Monad

newtype ID' r a = ID' {runID' :: (a -> r) -> r}

case' :: ID' r a -> (a -> r) -> r
case' = \x -> \f -> runID' x f

-- конструктор
constrID' :: a -> ID' r a
constrID' = \a -> ID' $ \f -> f a

-- инстансы
instance Functor (ID' r) where
    fmap f ma = ID' $ \g -> case' ma (\a -> g (f a))

instance Applicative (ID' r) where
    pure = constrID'
    mf <*> ma = ID' $ \g -> case' ma (\a -> case' mf (\f -> g (f a )))

instance Monad (ID' r) where
    return = constrID'
    ma >>= f = ID' $ \g -> case' ma (\a -> runID' (f a) g)

