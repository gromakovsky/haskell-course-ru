{-# LANGUAGE FlexibleInstances, RankNTypes #-}
module StateCPS where

import Control.Applicative
import Control.Monad

newtype State' s a = State' {runState' :: forall r . s -> (s -> a -> r) -> r}
newtype State'' s a = State'' {runState'' :: forall r . ((s -> (s, a)) -> r) -> r}

caseState' :: (State' s a) -> ((s -> (s, a)) -> r) -> r
caseState' = \x -> \f -> f $ \s -> runState' x s (\s -> \a -> (s, a))

caseState'' :: (State'' s a) -> ((s -> (s, a)) -> r) -> r
caseState'' = \x -> \f -> runState'' x f

-- конструктор
state' :: (s -> (s, a)) -> State' s a
state' st = State' $ \s -> \f -> let (s', a) = st s in f s' a

state'' :: (s -> (s, a)) -> State'' s a
state'' st = State'' $ \f -> f st

-- инстансы
instance Functor (State' s) where
    fmap f sa = State' $ \s -> \g -> caseState' sa (\st -> let (s', a) = st s in g s' (f a))

instance Functor (State'' s) where
    fmap f sa = State'' $ \g -> caseState'' sa (\st -> g $ (\s -> let (s', a) = st s in (s', f a)))

instance Applicative (State' s) where
    pure a = state' $ \s -> (s, a)
    sf <*> sa = State' $ \s -> \g -> caseState' sf (\stf -> let (s', f) = stf s in caseState' sa (\sta -> let (s'', a) = sta s' in g s'' (f a)))

instance Applicative (State'' s) where
    pure a = state'' $ \s -> (s, a)
    sf <*> sa = State'' $ \g -> caseState'' sf (\stf -> caseState'' sa (\sta -> g $ \s -> let (s', f) = stf s in let (s'', a) = sta s in (s'', f a)))

instance Monad (State' s) where
    return a = state' $ \s -> (s, a)
    sa >>= f = State' $ \s -> \g -> caseState' sa (\sta -> let (s', a) = sta s in runState' (f a) s' g)

instance Monad (State'' s) where
    return a = state'' $ \s -> (s, a)
    sa >>= f = State'' $ \g -> caseState'' sa (\sta -> g $ \s -> let (s', a) = sta s in runState'' (f a) (\stb -> stb s'))

