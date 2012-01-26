{-# LANGUAGE FlexibleContexts, FlexibleInstances, TypeSynonymInstances, ScopedTypeVariables, MultiParamTypeClasses #-}
{-# OPTIONS -Wall -fno-warn-orphans #-}
module StandardCoordinates.Dense(
    module QuadCoordinates.Dense,
    StandardDense,
    StandardDenseI,
    StandardDenseR,
    sd_fromVector,
    sd_toVector,
    sd_fromStandardCoords
    
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


type StandardDense v r = WrappedVector INormalDisc v r
type StandardDenseI = StandardDense V.Vector Integer
type StandardDenseR = StandardDense V.Vector Rational

sd_toVector :: StandardDense v r -> v r
sd_toVector (v :: StandardDense v r) = unwrapVector v

sd_fromVector :: VG.Vector v r => v r -> StandardDense v r
sd_fromVector (v :: v r) = 
    assert (mod (VG.length v) 7 == 0) $
    (WrappedVector v :: StandardDense v r)

instance (VG.Vector v r, Num r, Ord r, Ord (v r)) => QuadCoords (StandardDense v r) r where
    quadCount = default_quadCount_from_discCount
    quadAssocs = quadAssocsDistinct
    quadAssocsDistinct = default_quadAssocsDistinct_from_discAssocsDistinct

instance (VG.Vector v r, Num r, Ord r, Ord (v r)) => StandardCoords (StandardDense v r) r where
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

