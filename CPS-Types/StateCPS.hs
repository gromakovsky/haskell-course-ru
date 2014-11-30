{-# LANGUAGE FlexibleInstances #-}
module StateCPS where

import Control.Applicative
import Control.Monad

newtype State' r s a = State' {runState' :: ((s -> (s, a)) -> r) -> r}

caseState' :: (State' r s a) -> ((s -> (s, a)) -> r) -> r
caseState' = \x -> \f -> runState' x f

-- конструктор
constrState' :: (s -> (s, a)) -> State' r s a
constrState' st = State' $ \f -> f st

-- инстансы
instance Functor (State' r s) where
    fmap f sa = State' $ \g -> caseState' sa (\st -> g $ \s -> let (s', a) = st s in (s', f a))

instance Applicative (State' r s) where
    pure a = constrState' $ \s -> (s, a)
    sf <*> sa = undefined
--    sf <*> sa = State' $ \g -> caseState' sa (\st -> caseState' sf (\f -> g $ \s -> let (s', a) = st s in (s', f a)))

instance Monad (State' r s) where
    return a = constrState' $ \s -> (s, a)
    sa >>= f = undefined
--    sa >>= f = State' $ \g -> caseState' sa (\st -> g $ \s -> let (s', a) = st s in f a (\st' -> st' s'))

