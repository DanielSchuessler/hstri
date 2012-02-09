{-# LANGUAGE FlexibleInstances, NoMonomorphismRestriction, DeriveFoldable, DeriveFunctor, DeriveTraversable, GeneralizedNewtypeDeriving, TemplateHaskell, TypeFamilies #-}
{-# OPTIONS -Wall -fno-warn-missing-signatures #-}
module HomogenousTuples where

import TupleTH
import Data.List
import Prelude hiding((<=))
import qualified Prelude as P
import Data.Ord(comparing)
import Language.Haskell.TH
import Data.Foldable(Foldable,foldMap)
import Data.Traversable(Traversable)
import Control.Monad
import Control.Applicative
import THBuild
import Data.AdditiveGroup(AdditiveGroup(..))
import Data.VectorSpace(VectorSpace(..),InnerSpace(..))
import Data.Monoid(Sum(..))

type Pair a = (a,a)
type Triple a = (a,a,a)
type Quadruple a = (a,a,a,a)
type Sextuple a = (a,a,a,a,a,a)
type Septuple a = (a,a,a,a,a,a,a)
type Dodecatuple a = (a,a,a,a,a,a,a,a,a,a,a,a)

toList2 ::  (Pair t) -> [t]
toList2 (x1,x2) = [x1,x2]
toList3 ::  (Triple t) -> [t]
toList3 (x1,x2,x3) = [x1,x2,x3]
toList4 ::  (Quadruple t) -> [t]
toList4 (x1,x2,x3,x4) = [x1,x2,x3,x4]
toList6 :: (Sextuple a) -> [a]
toList6 = $(tupleToList 6)


isOrdered2 :: Ord a => (Pair a) -> Bool
isOrdered2 (v0,v1) = v0 < v1
isOrdered3 :: Ord a => (Triple a) -> Bool
isOrdered3 (v0,v1,v2) = v0 < v1 && v1 < v2 
isOrdered4 :: Ord a => (Quadruple a) -> Bool
isOrdered4 (v0,v1,v2,v3) = isOrdered3 (v0,v1,v2) && v2 < v3

isNondecreasing2 :: Ord a => (a, a) -> Bool
isNondecreasing2 (v0,v1) = v0 P.<= v1

list4 :: a -> b -> c -> d -> (a, b, c, d)
list4 = (,,,) 




map2 :: (a -> b) -> Pair a -> Pair b
map2 = $(mapTuple 2)

map3 :: (a -> b) -> Triple a -> Triple b
map3 = $(mapTuple 3)

map4 ::  (a -> b) -> (Quadruple a) -> (Quadruple b)
map4 = $(mapTuple 4)

map6 :: (a -> b) -> (Sextuple a) -> (Sextuple b)
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
sort2 = sort2By compare

sort2By :: (t -> t -> Ordering) -> (t, t) -> (t, t)
sort2By f xs@(x0,x1) = if f x0 x1 /= GT then xs else (x1,x0)

sort3 :: Ord t => (t, t, t) -> (t, t, t)
sort3 = sort3By compare

sort3By :: (t -> t -> Ordering) -> (t, t, t) -> (t, t, t)
sort3By f xs@(x0,x1,x2) =
    let
        x <= y = f x y /= GT
    in
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
sort4 =  $(tupleFromList 4) . sort . $(tupleToList 4)

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
    
sum3 :: Num a => Triple a -> a
sum3 = $(sumTuple 3)

sum4 :: Num a => Quadruple a -> a
sum4 = $(sumTuple 4)

zipTupleWith3
  :: (t -> t1 -> t2) -> Triple t -> Triple t1 -> Triple t2
zipTupleWith3 = $(zipTupleWith 3)

deleteAt2 :: (Eq i, Num i, Show i) => i -> (a,a) -> a
deleteAt2 = $(deleteAtTuple 2)

deleteAt3 :: (Eq i, Num i, Show i) => i -> (a,a,a) -> (a,a)
deleteAt3 = $(deleteAtTuple 3)

deleteAt4 :: (Eq i, Num i, Show i) => i -> (a,a,a,a) -> (a,a,a)
deleteAt4 = $(deleteAtTuple 4)

reverse3 :: (t2, t1, t) -> (t, t1, t2)
reverse3 = $(reverseTuple 3)

reverse2 :: (t1, t) -> (t, t1)
reverse2 = $(reverseTuple 2)

zipTuple2 :: (a, a1) -> (b, b1) -> ((a, b), (a1, b1))
zipTuple2 = $(zipTuple 2)

zipTuple3
  :: (a, a1, a2) -> (b, b1, b2) -> ((a, b), (a1, b1), (a2, b2))
zipTuple3 = $(zipTuple 3)

subtuples3_2 :: (t, t1, t2) -> ((t, t1), (t, t2), (t1, t2))
subtuples3_2 = $(subtuples 3 2)

subtuples4_2
  :: (t, t1, t2, t3)
     -> ((t, t1), (t, t2), (t, t3), (t1, t2), (t1, t3), (t2, t3))
subtuples4_2 = $(subtuples 4 2)

subtuples4_3
  :: (t, t1, t2, t3)
     -> ((t, t1, t2), (t, t1, t3), (t, t2, t3), (t1, t2, t3))
subtuples4_3 = $(subtuples 4 3)

foldlTuple4 :: (r -> t -> r) -> r -> (t, t, t, t) -> r
foldlTuple4 = $(foldlTuple 4) 


foldlMTuple4
  :: Monad m => (r -> b -> m r) -> m r -> (b, b, b, b) -> m r
foldlMTuple4 c = foldlTuple4 (\r x -> r >>= flip c x)

sort2On :: Ord a => (t -> a) -> (t, t) -> (t, t)
sort2On f = sort2By (comparing f)
sort3On :: Ord a => (t -> a) -> (t, t, t) -> (t, t, t)
sort3On f = sort3By (comparing f)

sortTuple :: Int -> ExpQ
sortTuple 0 = [| id |]
sortTuple 1 = [| id |]
sortTuple 2 = [| sort2 |]
sortTuple 3 = [| sort3 |]
sortTuple 4 = [| sort4 |]
sortTuple i = [| $(tupleFromList i) . sort . $(tupleToList i) |]

interpol :: (Num c, Applicative f) => c -> f c -> f c -> f c
interpol p = liftA2 (\x y -> (1-p) * x + p * y)

innerProductForFoldableApplicative x y = getSum . foldMap Sum $ (liftA2 (*) x y) 


$(liftM concat $ forM [2,3] (\n ->
    let
        na = mkName ("Tup"++show n)
        ctorname = na
        thetype = conT na
        a = mkName "a"

        n_vars stem = [ mkName ("_"++stem++show i) | i <- [0..n-1] ]
    in
        sequence
        [ snewtypeD (cxt[]) na (a&[]) (snormalC ctorname (htuple n (varT a))) 
            [''Functor,''Foldable,''Traversable,''Show,''Eq,''Ord ]

        , instanceD (cxt []) (''Applicative `sappT` thetype) 
            [
                svalD 'pure 
                    ("_x" \-> (ctorname `sappE` (stupE (replicate n "_x"))))
            ,   svalD '(<*>)
                    (sconP ctorname (stupP (n_vars "f")) \->
                     sconP ctorname (stupP (n_vars "x")) \->
                     sappE ctorname (tupE (zipWith sappE (n_vars "f") (n_vars "x"))))
            ]


        , svalD ("tup"++show n) (n_vars "x" \-> ctorname `sappE` (stupE (n_vars "x")))
        , svalD ("foldT"++show n) 
            (   "_k" \-> 
                sconP ctorname (stupP (n_vars "x")) \-> 
                foldl sappE (expQ "_k") (n_vars "x"))


        , instanceD 
            (cxt [sclassP ''Num "a"])
            (''AdditiveGroup `sappT` (thetype `sappT` "a"))
            [ svalD 'zeroV ('pure `sappE` integerL 0)
            , svalD '(^+^) ('liftA2 `sappE` '(+)) 
            , svalD 'negateV ('fmap `sappE` 'negate)
            ]

        , instanceD 
            (cxt [sclassP ''Num "a"])
            (''VectorSpace `sappT` (thetype `sappT` "a"))
            [ 
              stySynInstD ''Scalar [thetype `sappT` "a"] "a" 
            , svalD '(*^) ("p" \-> 'fmap `sappE` ("x" \-> '(*) `sappE` "p" `sappE` "x"))
            ]

        , instanceD 
            (cxt [sclassP ''Num "a"])
            (''InnerSpace `sappT` (thetype `sappT` "a"))
            [ 
              svalD '(<.>) 'innerProductForFoldableApplicative
            ]


        ]

    ))


tup2X = tup2 1 0
tup2Y = tup2 0 1
