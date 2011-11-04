{-# LANGUAGE BangPatterns, MultiParamTypeClasses, UndecidableInstances, FlexibleContexts, TypeFamilies, FlexibleInstances, StandaloneDeriving, GADTs, ScopedTypeVariables, DeriveDataTypeable, NoMonomorphismRestriction #-}
module List where
import Data.Typeable
import Control.Exception
import TypeLevel.TF

data List v n where
    Nil :: List v Z 
    (:::) :: v -> List v n -> List v (S n) 


infixr 5 :::

deriving instance Show v => Show (List v n)
deriving instance Eq v => Eq (List v n)
deriving instance Ord v => Ord (List v n)

lsingle v = v ::: Nil

-- class MakeList a where 
--     list :: a
-- 
-- instance MakeList (List v Z) where
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
type instance Pred (S n) = n

instance (MakeList' d, 

          MakeListElement d ~ v,
          S (Pred (MakeListN d)) ~ MakeListN d) => 
          
          MakeList' (v -> d) where

    type MakeListElement (v -> d) = v
    type MakeListN (v -> d) = Pred (MakeListN d)

    list' vs v = list' (lsnoc vs v)


lsnoc :: List v n -> v -> List v (S n)
lsnoc Nil v = v ::: Nil
lsnoc (v0 ::: vs) v = v0 ::: lsnoc vs v

-- type instance LowerCodomain v (List v (S n)) = List v n 
-- type instance LowerCodomain v (v -> d) = v -> LowerCodomain v d




data NegativeIndex = NegativeIndex Int String
    deriving (Show,Typeable)

instance Exception NegativeIndex

data IndexTooLarge = IndexTooLarge Int Int 
    deriving (Show,Typeable)

instance Exception IndexTooLarge

ldelete :: forall n v. Nat n => Int -> List v (S n) -> List v n
ldelete i | i < 0 = throw (NegativeIndex i "in ldelete")
          | len <- natToInt (undefined :: n), i > len =
                throw (IndexTooLarge i len)
          | otherwise = 
              let
                f :: forall n'. Int -> List v (S n') -> List v n'
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

lmap :: (a -> b) -> List a n -> List b n
lmap f Nil = Nil
lmap f (x ::: xs) = f x ::: lmap f xs

range :: (Enum a, Nat n) => a -> n -> List a n
range start n = caseNat n Nil (\n' -> start ::: range (succ start) n')

lhead :: List v (S n) -> v
lhead (x ::: _) = x

ltail :: List v (S n) -> List v n
ltail (_ ::: xs) = xs

-- lappend :: NatPair n n' => List v n -> List v n' -> List v (Plus n n')
-- lappend Nil ys = ys
-- lappend (x ::: xs) ys = x ::: (lappend xs ys)
--

lToTriple :: List t N3 -> (t, t, t)
lToTriple (x1 ::: x2 ::: x3 ::: Nil) = (x1,x2,x3)

lToQuadruple :: List t N4 -> (t, t, t, t)
lToQuadruple (x1 ::: x2 ::: x3 ::: x4 ::: Nil) = (x1,x2,x3,x4)


lfoldl' :: forall r v n. (r -> v -> r) -> r -> List v n -> r
lfoldl' c z = go z
    where
        go :: forall n. r -> List v n -> r
        go !acc Nil = acc
        go !acc (x:::xs) = go (acc `c` x) xs 


lfoldl1' :: forall r v n. (r -> r -> r) -> List r (S n) -> r
lfoldl1' c (z ::: xs) = go z xs
    where
        go :: forall n. r -> List r n -> r
        go !acc Nil = acc
        go !acc (x:::xs) = go (acc `c` x) xs 


