{-# LANGUAGE FlexibleInstances, Rank2Types #-}
module IdentityCPS where

import Control.Applicative
import Control.Monad

newtype ID' a = ID' {runID' :: forall r . (a -> r) -> r}

case' :: ID' a -> (a -> r) -> r
case' = \x -> \f -> runID' x f

-- конструктор
constrID' :: a -> ID' a
constrID' = \a -> ID' $ \f -> f a

-- инстансы
instance Functor ID' where
    fmap f ma = ID' $ \g -> case' ma (\a -> g (f a))

instance Applicative ID' where
    pure = constrID'
    mf <*> ma = ID' $ \g -> case' ma (\a -> case' mf (\f -> g (f a )))

instance Monad ID' where
    return = constrID'
    ma >>= f = ID' $ \g -> case' ma (\a -> runID' (f a) g)

