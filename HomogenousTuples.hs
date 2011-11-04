{-# LANGUAGE DeriveFoldable, DeriveFunctor, GeneralizedNewtypeDeriving, TemplateHaskell, TypeFamilies #-}
{-# OPTIONS -Wall #-}
module HomogenousTuples where

import TupleTH

type Pair a = (a,a)
type Triple a = (a,a,a)
type Quadruple a = (a,a,a,a)

toList2 ::  (t, t) -> [t]
toList2 (x1,x2) = [x1,x2]
toList3 ::  (t, t, t) -> [t]
toList3 (x1,x2,x3) = [x1,x2,x3]
toList4 ::  (t, t, t, t) -> [t]
toList4 (x1,x2,x3,x4) = [x1,x2,x3,x4]

map2 :: (a -> b) -> Pair a -> Pair b
map2 = $(mapTuple 2)

map3 :: (a -> b) -> Triple a -> Triple b
map3 = $(mapTuple 3)

map4 ::  (t -> t1) -> (t, t, t, t) -> (t1, t1, t1, t1)
map4 = $(mapTuple 4)

map6 :: (t2 -> t3) -> (t2, t2, t2, t2, t2, t2) -> (t3, t3, t3, t3, t3, t3)
map6 = $(mapTuple 6)

elem2 ::  Eq a => a -> (a, a) -> Bool
elem2 = $(elemTuple 2)
elem3 ::  Eq t1 => t1 -> (t1, t1, t1) -> Bool
elem3 = $(elemTuple 3)
elem4 ::  Eq t1 => t1 -> (t1, t1, t1, t1) -> Bool
elem4 = $(elemTuple 4)

all3 ::  (t -> Bool) -> (t, t, t) -> Bool
all3 = $(allTuple 3)
all4 ::  (t -> Bool) -> (t, t, t, t) -> Bool
all4 = $(allTuple 4)

any2 ::  (t -> Bool) -> (t, t) -> Bool
any2 = $(anyTuple 2)
any3 ::  (t1 -> Bool) -> (t1, t1, t1) -> Bool
any3 = $(anyTuple 3)
any4 ::  (t1 -> Bool) -> (t1, t1, t1, t1) -> Bool
any4 = $(anyTuple 4)


filter2 ::  (a -> Bool) -> (a, a) -> [a]
filter2 = $(filterTuple 2)
filter3 ::  (a1 -> Bool) -> (a1, a1, a1) -> [a1]
filter3 = $(filterTuple 3)
{-# INLINABLE filter4 #-}
filter4 ::  (a2 -> Bool) -> (a2, a2, a2, a2) -> [a2]
filter4 = $(filterTuple 4)
filter6 ::  (a3 -> Bool) -> (a3, a3, a3, a3, a3, a3) -> [a3]
filter6 = $(filterTuple 6)


fromList1 ::  [a] -> a
fromList1 = $(tupleFromList 1)
fromList2 ::  [t] -> (t, t)
fromList2 = $(tupleFromList 2)
fromList3 ::  [t1] -> (t1, t1, t1)
fromList3 = $(tupleFromList 3)
fromList4 ::  [t2] -> (t2, t2, t2, t2)
fromList4 = $(tupleFromList 4)


