{-# LANGUAGE TypeFamilies, RecordWildCards, TemplateHaskell, NamedFieldPuns, FlexibleContexts, TupleSections, NoMonomorphismRestriction, ScopedTypeVariables #-}
{-# OPTIONS -Wall #-}
module VectorUtil where
import qualified Data.Vector as V
import qualified Data.Vector.Generic as VG
import QuickCheckUtil
import Data.Maybe
import Test.QuickCheck
import TupleTH
import PrettyUtil.Matrix
import HomogenousTuples
import Element


vectorMapMaybe
  :: (VG.Vector v a, VG.Vector v1 a1) =>
     (a1 -> Maybe a) -> v1 a1 -> v a
vectorMapMaybe f xs = VG.unfoldrN (VG.length xs) u 0
    where
        u i = case xs VG.!? i of
                   Nothing -> Nothing
                   Just x -> case f x of
                                  Nothing -> u (i+1)
                                  Just y -> Just (y,i+1)

prop_mapMaybe :: Blind (Int -> Maybe Bool) -> [Int] -> Property
prop_mapMaybe (Blind (f :: Int -> Maybe Bool)) xs =
    mapMaybe f xs .=. (V.toList . vectorMapMaybe f . V.fromList) xs 

data PartitioningBySign v r = PartitioningBySign {   
    _Sneg, _S0, _Spos :: v r
}
    deriving Show

type instance Element (PartitioningBySign v r) = Element (v r) 

instance AsList (v r) => AsList (PartitioningBySign v r) where
    asList (PartitioningBySign a b c) = asList a ++ asList b ++ asList c

instance MaxColumnWidth (v r) => MaxColumnWidth (PartitioningBySign v r) where
    maxColumnWidth PartitioningBySign{..} =

         $(foldr1Tuple 3) max (map3 maxColumnWidth (_Sneg,_S0,_Spos))



{-# INLINABLE partitionBySign #-}
partitionBySign
  :: (Num b, Ord b) =>
     (r -> b) -> V.Vector r -> PartitioningBySign V.Vector r
partitionBySign f _Vp = PartitioningBySign {_Sneg,_S0,_Spos}
    where
                (_Sneg,_Snn ) = V.partition ((<0)  . f) _Vp
                (_S0  ,_Spos) = V.partition ((==0) . f) _Snn


vectorCart :: (VG.Vector v t, VG.Vector v a, VG.Vector v (t, a)) => v t -> v a -> v (t, a)
vectorCart v w = VG.concatMap (\x -> VG.map (x,) w) v

vectorAdjustAt :: VG.Vector v a => v a -> (a -> a) -> Int -> v a
vectorAdjustAt v f i = v VG.// [(i,f (v VG.! i))]

vectorAdjustAtF
  :: (Functor f, VG.Vector v a) =>
     v a -> (a -> f a) -> Int -> f (v a)
vectorAdjustAtF v f i =
    fmap (\y -> v VG.// [(i,y)]) (f (v VG.! i))
