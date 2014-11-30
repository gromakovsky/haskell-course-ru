{-# LANGUAGE FlexibleInstances #-}
module PairCPS where

newtype Pair' r a b = Pair' {runPair' :: (a -> b -> r) -> r}

case' :: Pair' r a b -> (a -> b -> r) -> r
case' = \x -> \f -> runPair' x f

-- конструктор
constrPair' :: a -> b -> Pair' r a b
constrPair' = \a -> \b -> Pair' $ \f -> f a b

-- инстансы вроде не придумать

