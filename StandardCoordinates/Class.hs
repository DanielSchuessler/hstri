{-# LANGUAGE TupleSections, UndecidableInstances, FlexibleInstances, TypeSynonymInstances, MultiParamTypeClasses, FunctionalDependencies, NoMonomorphismRestriction #-}
--{-# OPTIONS -Wall #-}
module StandardCoordinates.Class where

import INormalDisc
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


-- | 
-- Minimal definition: ('discCount' || 'triCount') && 'nsToAssocs'
--
-- Law: If more than the minimum is implemented, the impls must be equivalent to the default implementations
class QuadCoords s i => StandardCoords s i | s -> i where
    discCount :: s -> INormalDisc -> i 
    triCount :: s -> INormalTri -> i 

    -- | May (but need not) omit zero coefficients. May contain repeated discs.
    nsToAssocs :: s -> [(INormalDisc,i)]

    -- | May (but need not) omit zero coefficients. May contain repeated tris.
    triAssocs :: s -> [(INormalTri,i)]

    -- | May (but need not) omit zero coefficients.
    discAssocsDistinct :: s -> [(INormalDisc,i)]

    -- | May (but need not) omit zero coefficients.
    triAssocsDistinct :: s -> [(INormalTri,i)]


    discCount = eitherIND <$> triCount <*> quadCount
    triCount = (. iNormalDisc) <$> discCount

    triAssocs = mapMaybe (traverseFst (eitherIND Just (const Nothing))) . nsToAssocs
    nsToAssocs = (++) <$> (map (first iNormalDisc) . triAssocs) 
                      <*> (map (first iNormalDisc) . quadAssocs) 


    triAssocsDistinct = M.toList . M.fromListWith (+) . triAssocs
    discAssocsDistinct = M.toList . M.fromListWith (+) . nsToAssocs




-- | Superclass default; requires implementation of 'discCount'
defaultQuadCount :: StandardCoords s c => s -> INormalQuad -> c
defaultQuadCount = (. iNormalQuadToINormalDisc) <$> discCount

-- | Superclass default; requires implementation of 'nsToAssocs'
defaultQuadAssocs :: StandardCoords a t1 => a -> [(INormalQuad, t1)]
defaultQuadAssocs = mapMaybe (traverseFst (eitherIND (const Nothing) Just)) . nsToAssocs

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
    nsToAssocs d = [(d,1)] 

instance StandardCoords INormalTri Integer where
    discCount = discCount . iNormalDisc 
    nsToAssocs = nsToAssocs . iNormalDisc

instance StandardCoords INormalQuad Integer where
    discCount = discCount . iNormalDisc 
    nsToAssocs = nsToAssocs . iNormalDisc

instance (Num n, StandardCoords s n) => StandardCoords [s] n where
    discCount xs d = sum (flip discCount d <$> xs) 
    nsToAssocs = concatMap nsToAssocs 


instance (Num n, StandardCoords s n) => StandardCoords (FormalProduct n s) n where
    discCount (n :* s) = (n *) <$> discCount s

    nsToAssocs (n :* s) = second (n *) <$> nsToAssocs s


instance (Num n, StandardCoords s n, StandardCoords s' n) => StandardCoords (FormalSum s s') n where
    discCount (s :+ s') = (+) <$> discCount s <*> discCount s'

    nsToAssocs (s :+ s') = ((++) $ nsToAssocs s) $ nsToAssocs s'


eulerC
  :: (Fractional a, Integral n, StandardCoords s n) =>
     Triangulation -> s -> a
eulerC tr ns = sum (f <$> nsToAssocs ns)
    where
        f (d,n) = fromIntegral n*eitherIND ft fq d
        ft t = sum3 (map3 recipDeg (normalCorners t)) - 1/2
               - triArcCorrection t
        fq q = sum4 (map4 recipDeg (normalCorners q)) - 1
               - quadArcCorrection q
        
        recipDeg = recip . fromIntegral . degreeOfEdge . pMap tr . iNormalCornerGetContainingEdge

        (triArcCorrection,quadArcCorrection) = 
            if isClosedTriangulation tr
               then (const 0,const 0)
               else ( sum3 . map3 oneHalfIfBoundaryNormalArc . normalArcs
                    , sum4 . map4 oneHalfIfBoundaryNormalArc . normalArcs
                    )

        oneHalfIfBoundaryNormalArc na =
            if isBoundaryNormalArc . pMap tr $ na
               then 0.5
               else 0


eulerCRatio
  :: (StandardCoords s n, Integral n) =>
     Triangulation -> s -> Ratio n
eulerCRatio = eulerC

is2Sphere :: (Integral n, StandardCoords a n) => Triangulation -> a -> Bool
is2Sphere tr s = eulerCRatio tr s == 2 && isClosedSurface tr s

is2SphereOrDisk
  :: (Integral n, StandardCoords a n) => Triangulation -> a -> Bool
is2SphereOrDisk = (liftM2 . liftM2) (||) is2Sphere isDisk

-- surfaceBoundaryComponents tr s =
--     mkEquivalence0 (do
--         d <- normalDiscs s

isDisk tr s = assert False undefined

isClosedSurface tr s = assert False undefined

arcAssocs :: StandardCoords a t => a -> [(INormalArc, t)]
arcAssocs = concatMap (\(d,n) -> map (,n) (normalArcList d)) . nsToAssocs
