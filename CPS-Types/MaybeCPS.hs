{-# LANGUAGE FlexibleInstances #-}
module MaybeCPS where

import Control.Applicative
import Control.Monad

newtype Maybe' r a = Maybe' {runMaybe' :: (a -> r) -> r -> r}

caseMaybe' :: Maybe' r a -> (a -> r) -> r -> r
caseMaybe' = \x -> \f -> \g -> runMaybe' x f g

-- конструкторы
just :: a -> Maybe' r a
just a = Maybe' $ \f -> \g -> f a
nothing :: Maybe' r a
nothing = Maybe' $ \f -> \g -> g

-- инстансы
instance Functor (Maybe' r) where
    fmap f ma = Maybe' $ \g -> \h -> caseMaybe' ma (\a -> g (f a)) h

instance Applicative (Maybe' r) where
    pure = just
    mf <*> ma = Maybe' $ \g -> \h -> caseMaybe' ma (\a -> caseMaybe' mf (\f -> g $ f a) h) h

instance Monad (Maybe' r) where
    return = just
    ma >>= f = Maybe' $ \g -> \h -> caseMaybe' ma (\a -> runMaybe' (f a) g h) h

