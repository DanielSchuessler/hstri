{-# LANGUAGE FlexibleContexts, TupleSections, NoMonomorphismRestriction, ScopedTypeVariables #-}
{-# OPTIONS -Wall #-}
module VectorUtil where
import qualified Data.Vector as V
import qualified Data.Vector.Generic as VG
import QuickCheckUtil
import Data.Maybe
import Test.QuickCheck


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

{-# INLINABLE partitionBySign #-}
partitionBySign
  :: (Num b, Ord b) => (a -> b) -> V.Vector a -> (V.Vector a, V.Vector a, V.Vector a)
partitionBySign f _Vp = (_Sneg,_S0,_Spos)
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
