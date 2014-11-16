{-# LANGUAGE NoImplicitPrelude #-}
module ITMOPrelude.Categories where

-- Реализовать для всего,
-- что только можно из
import ITMOPrelude.Primitive
import ITMOPrelude.List
import ITMOPrelude.Tree
-- всевозможные инстансы для классов ниже

--------------------------------------------------------------------------------
-- Классы
class Category cat where
    id  :: cat a a
    (.) :: cat b c -> cat a b -> cat a c

class Functor f where
    fmap :: (a -> b) -> f a -> f b

class Monad m where
    return :: a -> m a
    (>>=) :: m a -> (a -> m b) -> m b

(>>) :: Monad m => m a -> m b -> m b
ma >> mb = ma >>= (\_ -> mb)

--------------------------------------------------------------------------------
-- Инстансы писать сюда

instance Functor (Pair a) where
    fmap f (Pair a b) = Pair a $ f b

instance Functor (Either a) where
    fmap f (Left a)  = Left a
    fmap f (Right b) = Right $ f b

instance Monad (Either a) where
    return = Right
    Left a >>= f  = Left a
    Right b >>= f = f b

instance Functor Maybe where
    fmap _ Nothing  = Nothing
    fmap f (Just x) = Just (f x)

instance Monad Maybe where
    return = Just
    Nothing >>= f  = Nothing
    (Just x) >>= f = f x

instance Functor List where
    fmap = map

instance Monad List where
    return x = Cons x Nil
    l >>= f = concatMap f l

instance Functor Tree where
    fmap = treeMap

instance Category (->) where
    id x = x
    f . g = \x -> f $ g x

--------------------------------------------------------------------------------
-- Монада State

newtype State s a = State { runState :: s -> (s, a) }

instance Monad (State s) where
    return a = State (\s -> (s, a))
    State run >>= f = State (\s -> let (s', a) = run s in runState (f a) s')

