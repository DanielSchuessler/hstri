{-# LANGUAGE MultiParamTypeClasses, UndecidableInstances, FlexibleContexts, TypeFamilies, FlexibleInstances, StandaloneDeriving, GADTs, ScopedTypeVariables, DeriveDataTypeable, NoMonomorphismRestriction #-}
module List where
import TypeLevel.NaturalNumber
import Data.Typeable
import Control.Exception
import Data.NaturalNumber(N(..))

data List v n where
    Nil :: List v Zero 
    (:::) :: v -> List v n -> List v (SuccessorTo n) 


infixr 5 :::

deriving instance Show v => Show (List v n)
deriving instance Eq v => Eq (List v n)
deriving instance Ord v => Ord (List v n)

lsingle v = v ::: Nil

-- class MakeList a where 
--     list :: a
-- 
-- instance MakeList (List v Zero) where
--     list = Nil
-- 
-- instance (MakeList' v b One) => MakeList (v -> b) where
--     list = list'

-- class (MakeList (LowerCodomain v b)) => MakeList' v b where
--     type LowerCodomain v b
--     list' :: v -> b

list = list' Nil

class MakeList' b where
    type MakeListElement b 
    type MakeListN b
    list' :: List (MakeListElement b) (MakeListN b) -> b

instance MakeList' (List v n) where
    type MakeListElement (List v n) = v
    type MakeListN (List v n) = n
    list' = id

type family Pred n
type instance Pred (SuccessorTo n) = n

instance (MakeList' d, 
          MakeListElement d ~ v,
          SuccessorTo (Pred (MakeListN d)) ~ MakeListN d) => 
          
          MakeList' (v -> d) where

    type MakeListElement (v -> d) = v
    type MakeListN (v -> d) = Pred (MakeListN d)

    list' vs v = list' (lsnoc vs v)


lsnoc :: List v n -> v -> List v (SuccessorTo n)
lsnoc Nil v = v ::: Nil
lsnoc (v0 ::: vs) v = v0 ::: lsnoc vs v

-- type instance LowerCodomain v (List v (SuccessorTo n)) = List v n 
-- type instance LowerCodomain v (v -> d) = v -> LowerCodomain v d




data NegativeIndex = NegativeIndex Int
    deriving (Show,Typeable)

instance Exception NegativeIndex

data IndexTooLarge = IndexTooLarge Int Int 
    deriving (Show,Typeable)

instance Exception IndexTooLarge

ldelete :: forall n v. NaturalNumber n => Int -> List v (SuccessorTo n) -> List v n
ldelete i | i < 0 = throw (NegativeIndex i)
          | len <- naturalNumberAsInt (undefined :: n), i > len =
                throw (IndexTooLarge i len)
          | otherwise = 
              let
                f :: forall n'. Int -> List v (SuccessorTo n') -> List v n'
                f i (v ::: vs) 
                        | i == 0 = vs
                        | otherwise = case vs of
                                        _ ::: _ -> 
                                            v ::: (f (i-1) vs) 

                                        Nil -> error "impossible"
              in f i


lToList :: List v n -> [v]
lToList Nil = []
lToList (x ::: xs) = x : lToList xs 


list1 = lsingle
list2 a b = a ::: list1 b
list3 a b c = a ::: list2 b c
list4 a b c d = a ::: list3 b c d

mapList :: (a -> b) -> List a n -> List b n
mapList f Nil = Nil
mapList f (x ::: xs) = f x ::: mapList f xs

range :: Enum a => a -> N n -> List a n
range start NZero = Nil
range start (NSuccessorTo n) = start ::: range (succ start) n
