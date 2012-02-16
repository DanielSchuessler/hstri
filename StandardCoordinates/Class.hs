{-# LANGUAGE DeriveDataTypeable, FlexibleContexts, EmptyDataDecls, ViewPatterns, CPP, ExistentialQuantification, TupleSections, UndecidableInstances, FlexibleInstances, TypeSynonymInstances, MultiParamTypeClasses, FunctionalDependencies, NoMonomorphismRestriction #-}
--{-# OPTIONS -Wall #-}
module StandardCoordinates.Class where

import Tetrahedron.INormalDisc
import Control.Applicative
import HomogenousTuples
import Triangulation
import Tetrahedron.NormalCorner
import TriangulationCxtObject
import Control.Arrow
import Data.Ratio
import QuadCoordinates.Class
import Data.Maybe
import Util
import Control.Monad
import Control.Exception
import qualified Data.Map as M
import Data.Map(Map)
import Math.SparseVector
import Triangulation.Class
import MathUtil
import Data.Typeable


-- | 
-- Minimal implementation: 
--
-- 'discCount' || 'triCount' && ' standardAsSparse '
--
-- Law: If more than the minimum is implemented, the impls must be equivalent to the default implementations
class QuadCoords s i => StandardCoords s i | s -> i where
    discCount :: s -> INormalDisc -> i 
    triCount :: s -> INormalTri -> i 

    -- | May (but need not) omit zero coefficients. May contain repeated discs.
    discAssocs :: s -> [(INormalDisc,i)]

    -- | May (but need not) omit zero coefficients. May contain repeated tris.
    triAssocs :: s -> [(INormalTri,i)]

    -- | May (but need not) omit zero coefficients.
    discAssocsDistinct :: s -> [(INormalDisc,i)]

    -- | May (but need not) omit zero coefficients.
    triAssocsDistinct :: s -> [(INormalTri,i)]

    standardAsSparse :: s -> SparseVector INormalDisc i

    discCount = default_discCount_from_triQuadCount
    triCount = default_triCount_from_discCount

    triAssocs = triAssocsDistinct
    discAssocs = discAssocsDistinct

    triAssocsDistinct = default_triAssocsDistinct_from_discAssocsDistinct
    discAssocsDistinct = sparse_toAssocs . standardAsSparse

class (StandardCoords s r, StandardCoords s' r) => UpdatableStandardCoords s s' r | s -> s' r where

    adjustTriCount :: (r -> r) -> INormalTri -> s -> s' 
    adjustDiscCount :: (r -> r) -> INormalDisc -> s -> s' 

default_discCount_from_triQuadCount
  :: StandardCoords s r => s -> INormalDisc -> r
default_discCount_from_triQuadCount = eitherIND <$> triCount <*> quadCount

default_triCount_from_discCount
  :: StandardCoords s c => s -> INormalTri -> c
default_triCount_from_discCount = (. iNormalTriToINormalDisc) <$> discCount

default_triAssocs_from_discAssocs :: StandardCoords a t1 => a -> [(INormalTri, t1)]
default_triAssocs_from_discAssocs = mapMaybe (traverseFst (eitherIND Just (const Nothing))) . discAssocs


default_discAssocs_from_triQuadAssocs :: StandardCoords a d => a -> [(INormalDisc, d)]
default_discAssocs_from_triQuadAssocs =
                  (++) <$> (map (first iNormalDisc) . triAssocs) 
                       <*> (map (first iNormalDisc) . quadAssocs) 


default_discAssocsDistinct_from_triQuadAssocsDistinct
  :: StandardCoords a d => a -> [(INormalDisc, d)]
default_discAssocsDistinct_from_triQuadAssocsDistinct =
                  (++) <$> (map (first iNormalDisc) . triAssocsDistinct) 
                       <*> (map (first iNormalDisc) . quadAssocsDistinct) 


default_triAssocsDistinct_from_triAssocs :: StandardCoords a a1 => a -> [(INormalTri, a1)]
default_triAssocsDistinct_from_triAssocs = M.toList . M.fromListWith (+) . triAssocs                 

default_discAssocsDistinct_from_discAssocs :: StandardCoords a a1 => a -> [(INormalDisc, a1)]
default_discAssocsDistinct_from_discAssocs = M.toList . M.fromListWith (+) . discAssocs



default_quadCount_from_discCount :: StandardCoords s r => s -> INormalQuad -> r
default_quadCount_from_discCount = (. iNormalQuadToINormalDisc) <$> discCount

default_quadAssocs_from_discAssocs :: StandardCoords a r => a -> [(INormalQuad, r)]
default_quadAssocs_from_discAssocs = onlyQuadAssocs . discAssocs

default_quadAssocsDistinct_from_discAssocsDistinct = onlyQuadAssocs . discAssocsDistinct

default_triAssocsDistinct_from_discAssocsDistinct
  :: StandardCoords a r => a -> [(INormalTri, r)]
default_triAssocsDistinct_from_discAssocsDistinct = onlyTriAssocs . discAssocsDistinct

default_discAssocsDistinct_from_standardAsSparse =
    sparse_toAssocs . standardAsSparse


-- The number of normal triangles in the first arg containing a normal arc of the given type.
-- Note that this is only well-defined in the disjoint union of tetrahedra, not in the quotient space!
numberOfTrisContainingArcType
  :: StandardCoords s i => s -> INormalArc -> i
numberOfTrisContainingArcType = (. iNormalTriByNormalArc) <$> triCount

numberOfQuadsContainingArcType
  :: StandardCoords s i => s -> INormalArc -> i
numberOfQuadsContainingArcType = (. iNormalQuadByNormalArc) <$> quadCount

numberOfArcsOfType :: StandardCoords s c => s -> INormalArc -> c
numberOfArcsOfType = 
    (liftA2 . liftA2) (+) numberOfTrisContainingArcType numberOfQuadsContainingArcType

numberOfCornersOfType
  :: StandardCoords s a => s -> INormalCorner -> a
numberOfCornersOfType ns nc =
    sum4
        (map4 
            (discCount ns)
            (iNormalDiscsContainingNormalCorner nc))


-- | In the same order as 'tINormalDiscs'
ns_toDenseAssocs
  :: StandardCoords s i => Triangulation -> s -> [(INormalDisc, i)]
ns_toDenseAssocs tr ns = fmap (id &&& discCount ns) (tINormalDiscs tr)

-- | In the same order as 'tINormalDiscs'
ns_toDenseList :: StandardCoords s i => Triangulation -> s -> [i]
ns_toDenseList tr = fmap snd . ns_toDenseAssocs tr 


instance StandardCoords INormalDisc Integer where
    discCount d d' = if d==d' then 1 else 0
    discAssocs = discAssocsDistinct
    discAssocsDistinct d = [(d,1)] 
    standardAsSparse = flip sparse_singleton 1

    triCount = default_triCount_from_discCount
    triAssocs = default_triAssocs_from_discAssocs
    triAssocsDistinct = triAssocs
    

instance StandardCoords INormalTri Integer where
    discCount = discCount . iNormalDisc 
    discAssocs = discAssocsDistinct
    discAssocsDistinct = discAssocsDistinct . iNormalDisc
    standardAsSparse = standardAsSparse . iNormalDisc

    triCount t t' = if t==t' then 1 else 0
    triAssocs = triAssocsDistinct
    triAssocsDistinct t = [(t,1)]

instance StandardCoords INormalQuad Integer where
    discCount = discCount . iNormalDisc 
    discAssocs = discAssocs . iNormalDisc
    discAssocsDistinct = discAssocsDistinct . iNormalDisc
    standardAsSparse = standardAsSparse . iNormalDisc

    triCount = const (const 0) 
    triAssocs = const []
    triAssocsDistinct = const []

instance (Num n, StandardCoords s n) => StandardCoords [s] n where
    discCount xs d = sum (flip discCount d <$> xs) 
    discAssocs = concatMap discAssocs 
    discAssocsDistinct = default_discAssocsDistinct_from_discAssocs
    standardAsSparse = sparse_sumWith (+) . map standardAsSparse

    triCount xs d = sum (flip triCount d <$> xs) 
    triAssocs = concatMap triAssocs 
    triAssocsDistinct = default_triAssocsDistinct_from_triAssocs

instance (Typeable n, Num n, StandardCoords s n) => StandardCoords (FormalProduct n s) n where
    discCount (n :* s) = (n *) <$> discCount s
    discAssocs (n :* s) = second (n *) <$> discAssocs s
    discAssocsDistinct (n :* s) = second (n *) <$> discAssocsDistinct s
    standardAsSparse (n :* s) = (n *) <$> standardAsSparse s

    triCount (n :* s) = (n *) <$> triCount s
    triAssocs (n :* s) = second (n *) <$> triAssocs s
    triAssocsDistinct (n :* s) = second (n *) <$> triAssocsDistinct s

instance (Num n, StandardCoords s n, StandardCoords s' n) => StandardCoords (FormalSum s s') n where
    discCount (s :+ s') = (+) <$> discCount s <*> discCount s'
    discAssocs (s :+ s') = discAssocs s ++ discAssocs s'
    discAssocsDistinct = default_discAssocsDistinct_from_discAssocs
    standardAsSparse (s :+ s') = sparse_addWith (+) (standardAsSparse s) (standardAsSparse s')

    triCount (s :+ s') = (+) <$> triCount s <*> triCount s'
    triAssocs (s :+ s') = triAssocs s ++ triAssocs s'
    triAssocsDistinct = default_triAssocsDistinct_from_triAssocs





arcAssocs :: StandardCoords a r => a -> [(INormalArc, r)]
arcAssocs = concatMap (\(d,n) -> map (,n) (normalArcList d)) . discAssocs

arcAssocsDistinct :: StandardCoords a r => a -> [(INormalArc, r)]
arcAssocsDistinct = M.toList . M.fromListWith (+) . arcAssocs

cornerAssocs :: StandardCoords a r => a -> [(INormalCorner, r)]
cornerAssocs = concatMap (\(d,n) -> map (,n) (normalCornerList d)) . discAssocs

cornerAssocsDistinct :: StandardCoords a r => a -> [(INormalCorner, r)]
cornerAssocsDistinct = M.toList . M.fromListWith (+) . cornerAssocs

--standardCoordsToSparse = sparse_from

data AnyStandardCoords i = 
    forall s. (StandardCoords s i) => AnyStandardCoords s

    deriving Typeable

instance Show (AnyStandardCoords i) where
    show _ = "<AnyStandardCoords>"



stc_extensionallyEq x y =
        standardAsSparse x == standardAsSparse y

stc_extensionalCompare x y =
        standardAsSparse x `compare` standardAsSparse y

-- | Extensional
instance Eq (AnyStandardCoords i) where
    (AnyStandardCoords x) == (AnyStandardCoords y) =
        stc_extensionallyEq x y

-- | Extensional
instance Ord (AnyStandardCoords i) where
    (AnyStandardCoords x) `compare` (AnyStandardCoords y) =
        stc_extensionalCompare x y

instance (Num r, Ord r) => NormalSurfaceCoefficients (AnyStandardCoords r) r



#define F(X) X (AnyStandardCoords s) = X s
instance (Typeable i, Num i, Ord i) => QuadCoords (AnyStandardCoords i) i where
    F(quadCount)
    F(quadAssocs)
    F(quadAssocsDistinct)

instance (Typeable i, Num i, Ord i) => StandardCoords (AnyStandardCoords i) i where
    F(discCount)
    F(discAssocs)
    F(discAssocsDistinct)
    F(triCount)
    F(triAssocs)
    F(triAssocsDistinct)
    F(standardAsSparse)
#undef F

onlyTriAssocs :: [(INormalDisc, r)] -> [(INormalTri, r)]
onlyTriAssocs = mapMaybe (traverseFst (eitherIND Just (const Nothing)))

instance (Typeable r, Num r, Ord r) => StandardCoords (SparseVector INormalDisc r) r where
    discCount = sparse_get
    discAssocs = discAssocsDistinct
    discAssocsDistinct = sparse_toAssocs
    standardAsSparse = id

    triAssocs = default_triAssocs_from_discAssocs 
    triAssocsDistinct = default_triAssocsDistinct_from_discAssocsDistinct




instance (Typeable r, Num r, Ord r) => 
    UpdatableStandardCoords 
        (SparseVector INormalDisc r) 
        (SparseVector INormalDisc r) 
        r
        
        where

    adjustTriCount f = sparse_adjust f . iNormalTriToINormalDisc
    adjustDiscCount f = sparse_adjust f

partialCanonicalPart
  :: UpdatableStandardCoords s s r => TVertex -> s -> s
partialCanonicalPart v s =
    let
        tris = vertexLinkingSurfaceTris v
        r = minimum . map (triCount s) $ tris
    in
        foldr (adjustTriCount (subtract r)) s tris 
        



standard_toFundEdgeSol
  :: (Integral i,
      RatioToIntegral sr si,
      NonNegScalable (Ratio i) sr,
      StandardCoords sr (Ratio i)) =>
     sr -> si
standard_toFundEdgeSol q =
    let
        denoms = fmap (denominator . snd) . discAssocsDistinct $ q 
    in
        fromMaybe (assert False undefined) .
            ratioToIntegral . scaleNonNeg (lcms denoms % 1) $ q
        



