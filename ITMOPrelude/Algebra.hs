{-# LANGUAGE NoImplicitPrelude, FlexibleInstances #-}
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

mconcat :: (Monoid a) => List a -> a
mconcat = foldr mappend mempty

-- Инстансы писать сюда
instance Monoid Unit where
    mempty      = Unit
    mappend _ _ = Unit

instance Group Unit where
    ginv _ = Unit

instance (Monoid a, Monoid b) => Monoid (Pair a b) where
    mempty      = Pair mempty mempty
    mappend a b = Pair {fst = fst a `mappend` fst b,
                        snd = snd b `mappend` snd b}

instance (Monoid a) => Monoid (Maybe a) where
    mempty                    = Just mempty
    mappend (Just a) (Just b) = Just $ mappend a b
    mappend _ _               = Nothing

-- Первый не Nothing, если есть
newtype First a = First { getFirst :: Maybe a}

instance Monoid (First a) where
    mempty                    = First Nothing
    mappend (First Nothing) x = x
    mappend x _               = x

-- Последний не Nothing, если есть
newtype Last a = Last { getLast :: Maybe a}

instance Monoid (Last a) where
    mempty                   = Last Nothing
    mappend x (Last Nothing) = x
    mappend _ x              = x

newtype Any = Any { getAny :: Bool }

instance Monoid Any where
    mempty                  = Any False
    mappend (Any a) (Any b) = Any $ a || b

newtype All = All { getAll :: Bool }

instance Monoid All where
    mempty                  = All True
    mappend (All a) (All b) = All $ a && b

-- Лексикографическое сравнение
instance Monoid Tri where
    mempty       = EQ
    mappend LT _ = LT
    mappend EQ a = a
    mappend GT _ = GT

-- Пример (лексикографически сравнить два списка одинаковой длины, testList определён выше):
testList2 = Cons natOne $ Cons natTwo $ Cons natTwo $ Const natZero Nil
mconcat $ zipWith natCmp testList testList2

newtype Sum a = Sum { getSum :: a }

instance Monoid (Sum Nat) where
    mempty                  = Sum natZero
    mappend (Sum a) (Sum b) = Sum $ a +. b

newtype Product a = Product { getProduct :: a }

instance Monoid (Product Nat) where
    mempty                          = Product natOne
    mappend (Product a) (Product b) = Product $ a *. b

instance Monoid (Sum Int) where
    mempty                  = Sum intZero
    mappend (Sum a) (Sum b) = Sum $ a .+. b

instance Group (Sum Int) where
    ginv = Sum . intNeg . getSum

instance Monoid (Product Int) where
    mempty                          = Product intOne
    mappend (Product a) (Product b) = Product $ a .*. b

instance Monoid (Sum Rat) where
    mempty                  = Sum ratZero
    mappend (Sum a) (Sum b) = Sum $ a %+ b

instance Group (Sum Rat) where
    ginv = Sum . ratNeg . getSum

instance Monoid (Product Rat) where
    mempty                          = Product ratOne
    mappend (Product a) (Product b) = Product $ a %* b

instance Group (Product Rat) where
    ginv = Product . ratInv . getProduct

instance Monoid (List a) where
    mempty = Nil
    mappend = (++)

