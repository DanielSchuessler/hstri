{-# LANGUAGE FlexibleContexts, EmptyDataDecls, FlexibleInstances, MultiParamTypeClasses, FunctionalDependencies #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE UndecidableInstances #-}
module QuadCoordinates.Class
    (module Tetrahedron.INormalDisc,
     module Data.FormalOps,
     module NormalSurfaceBasic,

     QuadCoords(..),
     quadSupport,
     quadDominates,
     quad_toFundEdgeSol,
     quad_toDenseAssocs,
     quad_toDenseList,


     -- * Misc
     onlyQuadAssocs,
     )


    where

import Control.Applicative
import Control.Arrow
import Control.Monad
import Data.AdditiveGroup
import Data.FormalOps
import Data.Map(Map)
import Data.Maybe
import Data.Monoid
import Tetrahedron.INormalDisc
import Math.SparseVector
import MathUtil
import Util
import qualified Data.Map as M
import Data.Ratio
import Control.Exception
import NormalSurfaceBasic
import Triangulation.PreTriangulation


-- | Minimal implementation:
--
-- 'quadAsSparse' || 'quadCount' && ('quadAssocs' || 'quadAssocsDistinct') 
class (NormalSurfaceCoefficients q r, Ord q) => QuadCoords q r | q -> r where
     quadCount :: q -> INormalQuad -> r
                                         
     -- | May (but need not) omit zero coefficients. May contain repeated quads.
     quadAssocs :: q -> [(INormalQuad,r)]

     -- | May (but need not) omit zero coefficients.
     quadAssocsDistinct :: q -> [(INormalQuad,r)]

     quadAsSparse :: q -> SparseVector INormalQuad r

     quadCount = sparse_get . quadAsSparse
     quadAssocs = quadAssocsDistinct
     quadAssocsDistinct = sparse_toAssocs . quadAsSparse 
     quadAsSparse = sparse_fromAssocs . quadAssocs

instance QuadCoords INormalQuad Integer where
    quadCount q q' = if q==q' then 1 else 0
    quadAssocsDistinct q = [(q,1)]
    quadAsSparse = flip sparse_singleton 1

instance QuadCoords INormalDisc Integer where
    quadCount = eitherIND quadCount quadCount
    quadAssocsDistinct = eitherIND quadAssocs quadAssocs
    quadAsSparse = eitherIND quadAsSparse quadAsSparse

-- | Constant zero; only needed as a superclass
instance QuadCoords INormalTri Integer where
    quadCount = const (const 0)
    quadAssocsDistinct = const []
    quadAsSparse = const sparse_zero

instance (Num n, QuadCoords q n) => QuadCoords [q] n where
    quadCount xs q' = sum (flip quadCount q' <$> xs) 
    quadAssocs = concatMap quadAssocs 
    quadAsSparse = sparse_sumWith (+) . map quadAsSparse

instance (Num n, QuadCoords q n) => QuadCoords (FormalProduct n q) n where
    quadCount (n :* q) = (n *) <$> quadCount q
    quadAssocs (n :* q) = second (n *) <$> quadAssocs q
    quadAssocsDistinct (n :* q) = second (n *) <$> quadAssocsDistinct q
    quadAsSparse (n :* q) = (n*) <$> quadAsSparse q 

instance (Num n, QuadCoords q n, QuadCoords q' n) => QuadCoords (FormalSum q q') n where
    quadCount = fmap evalFormalSum . bitraverseFormalSum quadCount quadCount
    quadAssocs = foldFormalSum (++) . bimapFormalSum quadAssocs quadAssocs
    quadAsSparse (a :+ b) = sparse_addWith (+) (quadAsSparse a) (quadAsSparse b)

instance (Ord i, Num i) => QuadCoords (SparseVector INormalQuad i) i where
    quadCount = sparse_get
    quadAssocsDistinct = sparse_toAssocs
    quadAsSparse = id

onlyQuadAssocs :: [(INormalDisc, t1)] -> [(INormalQuad, t1)]
onlyQuadAssocs = mapMaybe (traverseFst (eitherIND (const Nothing) Just))


instance (Ord i, Num i) => QuadCoords (SparseVector INormalDisc i) i where
    quadCount v q = sparse_get v (iNormalQuadToINormalDisc q)
    quadAssocs = quadAssocsDistinct
    quadAssocsDistinct = onlyQuadAssocs . sparse_toAssocs 
    quadAsSparse = sparse_fromDistinctAscList . onlyQuadAssocs . sparse_toAscList

-- | Set of components where the given vector is non-zero. May contain repeated quads.
quadSupport :: QuadCoords q r => q -> [INormalQuad]
quadSupport = mapMaybe (\(q,n) -> guard (n/=0) >> Just q) . quadAssocs

quadDominates
  :: (QuadCoords q r, QuadCoords q1 r1) => q -> q1 -> Bool
quadDominates x y =
    all 
        (\q -> quadCount x q /= 0)
        (quadSupport y)




quad_toFundEdgeSol
  :: (Integral i,
      RatioToIntegral qr qi,
      NonNegScalable (Ratio i) qr,
      QuadCoords qr (Ratio i)) =>
     qr -> qi
quad_toFundEdgeSol q =
    let
        denoms = fmap (denominator . snd) . quadAssocsDistinct $ q 
    in
        fromMaybe (assert False undefined) .
            ratioToIntegral . scaleNonNeg (lcms denoms % 1) $ q
        

quad_toDenseAssocs
  :: (PreTriangulation tr, QuadCoords q r) =>
     tr -> q -> [(INormalQuad, r)]
quad_toDenseAssocs tr qc = fmap (id &&& quadCount qc) (tINormalQuads tr)

quad_toDenseList
  :: (PreTriangulation tr, QuadCoords q r) => tr -> q -> [r]
quad_toDenseList tr = fmap snd . quad_toDenseAssocs tr


