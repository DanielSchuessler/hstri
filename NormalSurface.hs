{-# LANGUAGE UndecidableInstances, FlexibleInstances, TypeSynonymInstances, MultiParamTypeClasses, FunctionalDependencies, NoMonomorphismRestriction #-}
{-# OPTIONS -Wall #-}
module NormalSurface where
import INormalDisc
import Control.Applicative
import HomogenousTuples
import Control.Arrow((&&&))
import Triangulation


-- | 
-- Minimal definition: 'discCount' || ('triCount' && 'quadCount')
--
-- Law: If more than the minimum is implemented, the impls must be equivalent to the default implementations
class NormalSurface s i | s -> i where
    discCount :: s -> INormalDisc -> i 

    triCount :: s -> INormalTri -> i 
    quadCount :: s -> INormalQuad -> i 

    discCount = eitherIND <$> triCount <*> quadCount
    triCount = (. iNormalDisc) <$> discCount
    quadCount = (. iNormalDisc) <$> discCount

-- The number of normal triangles in the first arg containing a normal arc of the given type.
-- Note that this is only well-defined in the disjoint union of tetrahedra, not in the quotient space!
numberOfTrisContainingArcType
  :: NormalSurface s i => s -> INormalArc -> i
numberOfTrisContainingArcType = (. iNormalTriByNormalArc) <$> triCount

numberOfQuadsContainingArcType
  :: NormalSurface s i => s -> INormalArc -> i
numberOfQuadsContainingArcType = (. iNormalQuadByNormalArc) <$> quadCount

numberOfArcsOfType
  :: (Num i, NormalSurface s i) => s -> INormalArc -> i
numberOfArcsOfType = liftA2 (+) <$> numberOfTrisContainingArcType <*> numberOfQuadsContainingArcType

numberOfCornersOfType
  :: (Num i, NormalSurface s i) => s -> INormalCorner -> i
numberOfCornersOfType ns nc =
    sum4
        (map4 
            (discCount ns)
            (iNormalDiscsContainingNormalCorner nc))


-- | In the same order as 'tINormalDiscs'
ns_toDenseAssocs
  :: NormalSurface s i => Triangulation -> s -> [(INormalDisc, i)]
ns_toDenseAssocs tr ns = fmap (id &&& discCount ns) (tINormalDiscs tr)

-- | In the same order as 'tINormalDiscs'
ns_toDenseList :: NormalSurface s i => Triangulation -> s -> [i]
ns_toDenseList tr = fmap snd . ns_toDenseAssocs tr 

instance Num n => NormalSurface INormalDisc n where
    discCount d d' = if d==d' then 1 else 0

instance Num n => NormalSurface INormalTri n where
    discCount = discCount . iNormalDisc 

instance Num n => NormalSurface INormalQuad n where
    discCount = discCount . iNormalDisc 

instance (Num n, NormalSurface s n) => NormalSurface [s] n where
    discCount xs d = sum (flip discCount d <$> xs) 

data FormalProduct a b = a :* b
    deriving Show

infixl 7 :*

instance (Num n, NormalSurface s n) => NormalSurface (FormalProduct n s) n where
    discCount (n :* s) = (n *) <$> discCount s

data FormalSum a b = a :+ b
    deriving Show

infixl 6 :+

instance (Num n, NormalSurface s n, NormalSurface s' n) => NormalSurface (FormalSum s s') n where
    discCount (s :+ s') = (+) <$> discCount s <*> discCount s'
