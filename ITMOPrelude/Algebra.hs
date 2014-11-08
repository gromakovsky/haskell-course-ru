{-# LANGUAGE NoImplicitPrelude #-}
module ITMOPredule.Algebra where

-- Реализовать для всего,
-- что только можно из
import ITMOPrelude.Primitive
-- всевозможные инстансы для классов ниже 

-- Если не страшно, то реализуйте их и для
import ITMOPrelude.List
import ITMOPrelude.Tree

-- Классы
class Monoid a where
    mempty :: a
    mappend :: a -> a -> a

class Monoid a => Group a where
    ginv :: a -> a

-- Инстансы писать сюда
instance Monoid Unit where
    mempty = Unit
    mappend _ _ = Unit

instance Group Unit where
    ginv _ = Unit

instance (Monoid a, Monoid b) => Monoid (Pair a b) where
    mempty = Pair mempty mempty
    mappend a b = Pair {fst = fst a `mappend` fst b,
                        snd = snd b `mappend` snd b}

instance (Monoid a) => Monoid (Maybe a) where
    mempty = Just mempty
    mappend (Just a) (Just b) = Just $ mappend a b
    mappend _ _ = Nothing

instance Monoid Bool where
    mempty = False
    mappend = (||)

instance Monoid Nat where
    mempty = natZero
    mappend = (+.)

instance Monoid Int where
    mempty = intZero
    mappend = (.+.)

instance Group Int where
    ginv = intNeg

instance Monoid Rat where
    mempty = ratZero
    mappend = (%*)

instance Group Rat where
    ginv = ratInv

instance Monoid (List a) where
    mempty = Nil
    mappend = (++)

