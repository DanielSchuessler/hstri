{-# LANGUAGE UndecidableInstances, FlexibleInstances, TypeSynonymInstances, MultiParamTypeClasses, FunctionalDependencies, NoMonomorphismRestriction #-}
{-# OPTIONS -Wall #-}
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

    discCount = eitherIND <$> triCount <*> quadCount
    triCount = (. iNormalDisc) <$> discCount

    triAssocs = mapMaybe (traverseFst (eitherIND Just (const Nothing))) . nsToAssocs
    nsToAssocs = (++) <$> (map (first iNormalDisc) . triAssocs) 
                      <*> (map (first iNormalDisc) . quadAssocs) 


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
numberOfArcsOfType = liftA2 (+) <$> numberOfTrisContainingArcType <*> numberOfQuadsContainingArcType

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
        fq q = sum4 (map4 recipDeg (normalCorners q)) - 1
        
        recipDeg = recip . fromIntegral . degreeOfEdge . pMap tr . iNormalCornerGetContainingEdge


eulerCRatio
  :: (StandardCoords s n, Integral n) =>
     Triangulation -> s -> Ratio n
eulerCRatio = eulerC

is2Sphere :: (Integral n, StandardCoords a n) => Triangulation -> a -> Bool
is2Sphere tr = (==2) . eulerCRatio tr
