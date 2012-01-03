{-# OPTIONS -Wall #-}
module Numbering where

import qualified Data.Map as M
import qualified Data.Vector as V
import Control.Arrow
import Test.QuickCheck
import Control.Applicative
import QuickCheckUtil

-- | A bijection from a subset of @a@ to @{ 0, ..., n-1}@ for some @n@
data Numbering a = Numbering {
    numbering_a2i :: M.Map a Int,
    numbering_i2a :: V.Vector a
    -- Invariant: These are inverses (with the Vector regarded as :: Int -> a)
}
    deriving (Show)

propIndexingInvariant1 :: (Eq a, Show a) => Numbering a -> Property
propIndexingInvariant1 (Numbering m v) = 
    forAllElements (M.assocs m)
    (\(a,i) -> v V.! i == a)

propIndexingInvariant2 :: Ord a => Numbering a -> Property
propIndexingInvariant2 (Numbering m v) = 
    forAllElements [0..V.maxIndex v]
    (\i -> m M.! (v V.! i) == i)

fromDistinctList :: Ord a => [a] -> Numbering a
fromDistinctList xs = Numbering m v 
    where
        m = M.fromListWith err (zip xs [0..]) 
        v = V.fromList xs
        err i1 i2 = error ("Numbering.fromDistinctList: duplicate at positions "++show (i1,i2))

toList :: Numbering k -> [k]
toList = V.toList . numbering_i2a

toInt :: Ord k => Numbering k -> k -> Int
toInt t x = numbering_a2i t M.! x

fromInt :: Numbering a -> Int -> a
fromInt t i = numbering_i2a t V.! i

length ::  Numbering a -> Int
length = V.length . numbering_i2a

-- | The third and fourth arg must both be strictly increasing, and every image of the third arg must be strictly smaller than every image of the fourth arg
sumNumbering
  :: Numbering a1
     -> Numbering a2 -> (a1 -> a) -> (a2 -> a) -> Numbering a
sumNumbering (Numbering m1 v1) (Numbering m2 v2) inl inr
    = Numbering 
        (M.fromDistinctAscList (l1++l2)) 
        (V.map inl v1 V.++ V.map inr v2) 
  where
    l1 = first inl <$> M.toAscList m1
    l2 = (inr *** (+n)) <$> M.toAscList m2
    n = V.length v1
    



productNumbering
  :: Ord a =>
     Numbering k -> Numbering a1 -> (k -> a1 -> a) -> Numbering a
productNumbering (Numbering m1 v1) (Numbering m2 v2) prod =
        Numbering
            (M.fromList [  (prod a1 a2, n2*i1 + i2)
                         |  (a1,i1) <- M.toList m1,
                            (a2,i2) <- M.toList m2
                        ])

            (V.concatMap 
                (\a1 -> V.map (prod a1) v2)
                v1)

    where
        n2 = V.length v2
        



enumNumbering :: (Enum a, Ord a) => a -> a -> Numbering a
enumNumbering min_ max_ =
    let
        mini = fromEnum min_

        toInt_ = subtract mini . fromEnum

        v = V.enumFromTo min_ max_
    in
        Numbering 
            (M.fromList (map (id &&& toInt_) (V.toList v))) 
            v

