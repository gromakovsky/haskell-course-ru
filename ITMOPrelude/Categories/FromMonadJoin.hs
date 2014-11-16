{-# LANGUAGE NoImplicitPrelude, FlexibleInstances, UndecidableInstances #-}
module ITMOPrelude.Categories.ToMonadJoin where
import ITMOPrelude.Categories.MonadJoin

-- Эти
import ITMOPrelude.Categories
import ITMOPrelude.Categories.MonadFish

-- делаем из нас

import ITMOPrelude.Primitive

instance MonadJoin m => Monad m where
    return  = returnJoin
    x >>= f = join (fmap f x)

instance MonadJoin m => MonadFish m where
    returnFish  = returnJoin
    f >=> g     = \a -> join $ fmap g $ f a

