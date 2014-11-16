{-# LANGUAGE NoImplicitPrelude, FlexibleInstances, UndecidableInstances #-}
module ITMOPrelude.Categories.ToMonad where
import ITMOPrelude.Categories

-- Эти
import ITMOPrelude.Categories.MonadJoin
import ITMOPrelude.Categories.MonadFish

-- делаем из нас

import ITMOPrelude.Primitive

instance Monad m => Functor m where
    fmap f x = x >>= \x -> return $ f x
 
instance Monad m => MonadJoin m where
    returnJoin  = return
    join x      = x >>= (\x -> x)

instance Monad m => MonadFish m where
    returnFish  = return
    f >=> g     = \a -> f a >>= g

