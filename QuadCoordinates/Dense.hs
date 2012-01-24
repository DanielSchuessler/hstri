{-# LANGUAGE NoMonomorphismRestriction, TypeFamilies, FlexibleContexts, FlexibleInstances, MultiParamTypeClasses #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# OPTIONS -Wall #-}
module QuadCoordinates.Dense where

import OrphanInstances()
import qualified Data.Vector.Generic as VG
import qualified Data.Vector as V
import QuadCoordinates.Class
import Data.VectorSpace
import Control.Exception
import PrettyUtil
import Triangulation
import qualified Data.Vector.Unboxed as VU


newtype WrappedVector tag v r = WrappedVector (v r)
    deriving(Eq,Ord,Pretty)

wv_under
  :: (t1 t2 -> v r) -> WrappedVector t t1 t2 -> WrappedVector tag v r
wv_under f (WrappedVector x) = WrappedVector (f x) 

wv_under2
  :: (t1 t2 -> t4 t5 -> v r)
     -> WrappedVector t t1 t2
     -> WrappedVector t3 t4 t5
     -> WrappedVector tag v r
wv_under2 f (WrappedVector x) (WrappedVector y) = WrappedVector (f x y)

instance Show r => Show (QuadDense V.Vector r) where
    showsPrec prec x = showString "qd_" . showsPrec prec (unwrapVector x)

instance (VU.Unbox r, Show r) => Show (QuadDense VU.Vector r) where
    showsPrec prec x = showString "qd_fromListU" . showsPrec prec (VU.toList (unwrapVector x))

instance (VG.Vector v r, Num r) => AdditiveGroup (WrappedVector tag v r) where
    zeroV = assert False undefined -- WrappedVector (VG.singleton v 0)
    (^+^) = wv_under2 (VG.zipWith (+)) 
    negateV = wv_under (VG.map negate)


instance (VG.Vector v r, Num r) => VectorSpace (WrappedVector tag v r) where
    type Scalar (WrappedVector tag v r) = r

    (*^) r = wv_under (VG.map (r*))

unwrapVector :: WrappedVector tag v r -> v r
unwrapVector (WrappedVector v) = v
    

type QuadDense v r = WrappedVector INormalQuad v r

type QuadDenseI = QuadDense V.Vector Integer
type QuadDenseR = QuadDense V.Vector Rational

qd_toVector :: QuadDense v r -> v r
qd_toVector = unwrapVector

qd_fromVector :: VG.Vector v r => v r -> QuadDense v r
qd_fromVector v = 
    assert (mod (VG.length v) 3 == 0) $
    WrappedVector v

instance (VG.Vector v r, Num r) => QuadCoords (QuadDense v r) r where
    quadCount v i = qd_toVector v VG.! fromEnum i
    quadAssocs = filter ((/= 0) . snd) . zip [toEnum 0 ..] . VG.toList . qd_toVector

qd_fromQuadCoords
  :: QuadCoords q r => Triangulation -> q -> QuadDense V.Vector r
qd_fromQuadCoords tr x =
    qd_fromVector $ V.generate (tNumberOfNormalQuadTypes tr) (quadCount x . toEnum)

qd_fromList :: [r] -> QuadDense V.Vector r
qd_fromList = qd_fromVector . V.fromList

qd_fromListU :: VU.Unbox r => [r] -> QuadDense VU.Vector r
qd_fromListU = qd_fromVector . VU.fromList
