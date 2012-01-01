{-# LANGUAGE NoMonomorphismRestriction, DeriveFoldable, DeriveFunctor, GeneralizedNewtypeDeriving, TemplateHaskell, TypeFamilies #-}
{-# OPTIONS -Wall #-}
module HomogenousTuples where

import TupleTH
import Data.List

type Pair a = (a,a)
type Triple a = (a,a,a)
type Quadruple a = (a,a,a,a)
type Sextuple a = (a,a,a,a,a,a)
type Septuple a = (a,a,a,a,a,a,a)
type Dodecatuple a = (a,a,a,a,a,a,a,a,a,a,a,a)

toList2 ::  (t, t) -> [t]
toList2 (x1,x2) = [x1,x2]
toList3 ::  (t, t, t) -> [t]
toList3 (x1,x2,x3) = [x1,x2,x3]
toList4 ::  (t, t, t, t) -> [t]
toList4 (x1,x2,x3,x4) = [x1,x2,x3,x4]
toList6 :: (a,a,a,a,a,a) -> [a]
toList6 = $(tupleToList 6)


isOrdered2 :: Ord a => (a, a) -> Bool
isOrdered2 (v0,v1) = v0 < v1
isOrdered3 :: Ord a => (a, a, a) -> Bool
isOrdered3 (v0,v1,v2) = v0 < v1 && v1 < v2 
isOrdered4 :: Ord a => (a, a, a, a) -> Bool
isOrdered4 (v0,v1,v2,v3) = isOrdered3 (v0,v1,v2) && v2 < v3

isNondecreasing2 :: Ord a => (a, a) -> Bool
isNondecreasing2 (v0,v1) = v0 <= v1

list4 :: a -> b -> c -> d -> (a, b, c, d)
list4 = (,,,) 




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

sort2 :: Ord t1 => (t1, t1) -> (t1, t1)
sort2 xs@(x0,x1) = if x0<=x1 then xs else (x1,x0)

sort3 :: Ord t => (t, t, t) -> (t, t, t)
sort3 xs@(x0,x1,x2) =
    if x0<=x1
       then 
        if x1<=x2
           then xs
           else -- x2 < x1 
            if x0<=x2
               then (x0,x2,x1)
               else -- x2 < x0 
                (x2,x0,x1)
       else -- x1 < x0
        if x0<=x2
           then (x1,x0,x2)
           else -- x2 < x0
            if x1<=x2
               then (x1,x2,x0)
               else -- x2 < x1
                (x2,x1,x0)

prop_sort3 :: (Int,Int,Int) -> Bool
prop_sort3 is = sort3 is == sort3' is
    where
        sort3' = $(tupleFromList 3) . sort . $(tupleToList 3)

sort4 :: Ord t => (t, t, t, t) -> (t, t, t, t)
sort4 = $(tupleFromList 4) . sort . $(tupleToList 4)


rotate4_1 :: (t1, t2, t3, t) -> (t, t1, t2, t3)
rotate4_1 = $(rotateTuple 4 1)


deleteTuple3 :: Eq a => a -> (a, a, a) -> Maybe (a, a)
deleteTuple3 = $(safeDeleteTuple 3)

deleteTuple4 :: Eq a => a -> (a, a, a, a) -> Maybe (a, a, a)
deleteTuple4 = $(safeDeleteTuple 4)


{-# SPECIALIZE indexInTriple :: (Show t, Eq t) => t -> (t,t,t)-> Int #-}
-- | Returns the first index if more than one element of the triple is equal to the element. Errors if none is.
indexInTriple ::  (Show t, Num a, Eq t) => t -> (t, t, t) -> a
indexInTriple x (x1,x2,x3) | x == x1 = 0
                           | x == x2 = 1
                           | x == x3 = 2
                           | otherwise = error ( unwords [ "indexInTriple" , show x, show (x1,x2,x3) ] )


intersect2_2 :: Eq a => (a, a) -> (a, a) -> [a]
intersect2_2 xs ys = filter2 (`elem2` ys) xs 
    
