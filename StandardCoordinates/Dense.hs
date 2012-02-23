{-# LANGUAGE NoMonomorphismRestriction, ViewPatterns, FlexibleContexts, FlexibleInstances, TypeSynonymInstances, ScopedTypeVariables, MultiParamTypeClasses #-}
{-# OPTIONS -Wall -fno-warn-orphans #-}
module StandardCoordinates.Dense(
    module QuadCoordinates.Dense,
    StandardDense,
    StandardDenseI,
    StandardDenseR,
    sd_fromVector,
    sd_fromList,
    sd_fromListU,
    sd_toVector,
    sd_fromStandardCoords,
    sd_zeroSet,
    sd_projectiveImage,
    sd_map
    
    ) 
    where

import QuadCoordinates.Dense
import StandardCoordinates.Class
import QuadCoordinates.Class
import qualified Data.Vector as V
import Control.Exception
import qualified Data.Vector.Generic as VG
import Triangulation
import Math.SparseVector
import qualified Data.Vector.Unboxed as VU
import Data.List
import Data.BitVector.Adaptive
import Data.Typeable


type StandardDense v r = WrappedVector StdCoordSys v r
type StandardDenseI = StandardDense V.Vector Integer
type StandardDenseR = StandardDense V.Vector Rational

sd_toVector :: StandardDense v r -> v r
sd_toVector (v :: StandardDense v r) = unwrapVector v

sd_fromVector :: VG.Vector v r => v r -> StandardDense v r
sd_fromVector (v :: v r) = 
    assert (mod (VG.length v) 7 == 0) $
    (WrappedVector v :: StandardDense v r)

instance (VG.Vector v r, Num r, Ord r, Ord (v r), Typeable1 v, Typeable r) => QuadCoords (StandardDense v r) r where
    quadCount = default_quadCount_from_discCount
    quadAssocs = quadAssocsDistinct
    quadAssocsDistinct = default_quadAssocsDistinct_from_discAssocsDistinct

instance (VG.Vector v r, Num r, Ord r, Ord (v r), Typeable1 v, Typeable r) => StandardCoords (StandardDense v r) r where
    discCount v i = sd_toVector v VG.! fromEnum i
    discAssocs = discAssocsDistinct
    discAssocsDistinct = filter ((/= 0) . snd) . zip [toEnum 0 ..] . VG.toList . sd_toVector
    standardAsSparse = sparse_fromDistinctAscList . discAssocsDistinct

    triCount = default_triCount_from_discCount
    triAssocs = triAssocsDistinct
    triAssocsDistinct = default_triAssocsDistinct_from_discAssocsDistinct

sd_fromStandardCoords
  :: StandardCoords s r =>
     Triangulation -> s -> StandardDense V.Vector r
sd_fromStandardCoords tr x =
    sd_fromVector $ V.generate (tNumberOfNormalDiscTypes tr) (discCount x . toEnum)

instance 
    (VG.Vector v r, Num r, Ord r, Ord (v r), Typeable1 v, Typeable r) =>
    UpdatableStandardCoords 
        (StandardDense v r)
        (StandardDense v r)
        r


    where 
        adjustTriCount f = adjustDiscCount f . iNormalTriToINormalDisc 
            
        adjustDiscCount f (fromEnum -> i) (WrappedVector v) =

                    WrappedVector (v VG.// [(i, f (v VG.! i))])

            
sd_fromList :: [r] -> StandardDense V.Vector r
sd_fromList = sd_fromVector . V.fromList

sd_fromListU :: VU.Unbox r => [r] -> StandardDense VU.Vector r
sd_fromListU = sd_fromVector . VU.fromList

instance Show r => Show (StandardDense V.Vector r) where
    showsPrec prec x = showString "sd_" . showsPrec prec (unwrapVector x)

instance (VU.Unbox r, Show r) => Show (StandardDense VU.Vector r) where
    showsPrec prec x = showString "sd_fromListU" . showsPrec prec (VU.toList (unwrapVector x))


{-# INLINE sd_zeroSet #-}
sd_zeroSet
  :: (Eq a1, Num a1, VG.Vector v a1, BitVector w) => StandardDense v a1 -> w
sd_zeroSet (sd_toVector -> v) =   
    v `seq`
    foldl'  (\r i -> if VG.unsafeIndex v i == 0
                       then bvSetBit r i
                       else r) 
            (bvEmpty n)
            [0..n-1] 

    where
        n = VG.length v



sd_projectiveImage
  :: (Eq r, Fractional r, Show (v r), VG.Vector v r) =>
     StandardDense v r -> StandardDense v r
sd_projectiveImage = wv_projectiveImage 


sd_map
  :: (VG.Vector v t2, VG.Vector v r) =>
     (t2 -> r) -> StandardDense v t2 -> StandardDense v r
sd_map f = wv_under (VG.map f)
