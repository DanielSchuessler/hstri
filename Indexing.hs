{-# OPTIONS -Wall #-}
module Indexing where

import qualified Data.Map as M
import qualified Data.Vector as V
import Control.Arrow
import Test.QuickCheck
import Control.Applicative

data Indexing a = Indexing {
    indexing_map :: M.Map a Int,
    indexing_v ::   V.Vector a
    -- Invariant: These are inverses (with the Vector regarded as :: Int -> a)
}
    deriving (Show)

propIndexingInvariant1 :: (Eq a, Show a) => Indexing a -> Property
propIndexingInvariant1 (Indexing m v) | M.null m = property (V.null v)
propIndexingInvariant1 (Indexing m v) = 
    forAll (elements (M.assocs m))
    (\(a,i) -> v V.! i == a)

propIndexingInvariant2 :: Ord a => Indexing a -> Property
propIndexingInvariant2 (Indexing m v) | V.null v = property (M.null m)
propIndexingInvariant2 (Indexing m v) = 
    forAll (elements [0..V.maxIndex v])
    (\i -> m M.! (v V.! i) == i)

fromDistinctList :: Ord a => [a] -> Indexing a
fromDistinctList xs = Indexing m v 
    where
        m = M.fromListWith err (zip xs [0..]) 
        v = V.fromList xs
        err i1 i2 = error ("Indexing.fromDistinctList: duplicate at positions "++show (i1,i2))

toList :: Indexing k -> [k]
toList = V.toList . indexing_v

toInt :: Ord k => Indexing k -> k -> Int
toInt t x = indexing_map t M.! x

fromInt :: Indexing a -> Int -> a
fromInt t i = indexing_v t V.! i

length ::  Indexing a -> Int
length = V.length . indexing_v

disjointUnionIndexing :: Indexing b -> Indexing c -> Indexing (Either b c)
disjointUnionIndexing (Indexing m1 v1) (Indexing m2 v2)
    = Indexing (M.fromDistinctAscList (l1++l2)) (V.map Left v1 V.++ V.map Right v2) 
  where
    l1 = first Left <$> M.toAscList m1
    l2 = (Right *** (+n)) <$> M.toAscList m2
    n = V.length v1
    




