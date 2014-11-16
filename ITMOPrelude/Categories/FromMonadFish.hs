{-# LANGUAGE NoImplicitPrelude, FlexibleInstances, UndecidableInstances #-}
module ITMOPrelude.Categories.ToMonadFish where
import ITMOPrelude.Categories.MonadFish

-- Эти
import ITMOPrelude.Categories
import ITMOPrelude.Categories.MonadJoin

-- делаем из нас

import ITMOPrelude.Primitive

instance MonadFish m => Monad m where
    return  = returnFish
    a >>= f = (\a -> a) >=> f $ a

instance MonadFish m => Functor m where
    fmap f a = (\a -> a) >=> (\a -> returnFish $ f a) $ a

instance MonadFish m => MonadJoin m where
    returnJoin  = returnFish
    join x      = (\a -> a) >=> (\a -> a) $ x

