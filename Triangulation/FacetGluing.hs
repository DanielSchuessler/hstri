{-# LANGUAGE StandaloneDeriving, GeneralizedNewtypeDeriving, FlexibleContexts, TemplateHaskell, ScopedTypeVariables, MultiParamTypeClasses, FunctionalDependencies, TupleSections, NoMonomorphismRestriction, TypeFamilies, FlexibleInstances, ViewPatterns #-} 
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS -Wall #-}
module Triangulation.FacetGluing where

import Tetrahedron
import Control.Applicative
import HomogenousTuples
import Tetrahedron.INormalDisc
import Tetrahedron.NormalArc
import Control.Exception
import Test.QuickCheck
import THUtil
import PrettyUtil
import PreRenderable.TriangleLabel
import Control.Arrow
import ShortShow
import Data.SumType
import DisjointUnion

type Gluing = (ITriangle,OITriangle)

gluing :: ITriangle -> OITriangle -> Gluing
gluing = (,)

gluing' :: ITriangle -> ITriangle -> Gluing
gluing' t = gluing t . toOrderedFace

-- class GluingSpec a where
--     type TetIndex a
--     toGluing :: (TetIndex a -> TIndex) -> a -> Gluing
-- 
-- instance GluingSpec (t,Triangle,t,S3,Triangle) where
--     type TetIndex (t,Triangle,t,S3,Triangle) = t
--     toGluing f (tt1,t1,tt2,g,t2) = (f tt1 ./ t1, f tt2 ./ packOrderedFace g t2)
-- 
-- instance GluingSpec (t,Triangle,t,OTriangle) where
--     type TetIndex (t,Triangle,t,OTriangle) = t
--     toGluing f (tt1,t1,tt2,t2) = (f tt1 ./ t1, f tt2 ./ t2)


oiTriangleGluing :: OITriangle -> OITriangle -> Gluing
oiTriangleGluing (unpackOrderedFace -> (tri,g)) otri = (tri,otri *. inv g)


-- | Creates a gluing equivalent to the input gluing but with the 'ITriangle's swapped and the permutation inverted
flipGluing :: Gluing -> Gluing
flipGluing (x1, unpackOrderedFace -> (x2,g)) = (x2, packOrderedFace x1 (inv g))

class GluingMappable a where
    gluingMap :: Gluing -> a -> a

gluingUnmap :: GluingMappable a => Gluing -> a -> a
gluingUnmap g a = gluingMap (flipGluing g) a

instance GluingMappable IVertex where

    gluingMap gl@(t,ot) v = 
        maybe 
            (error ("gluingMap "++show gl++" "++show v))
            (triangleGetVertexAt ot)  
            (triangleGetIndexOf t v)

-- | The input 'Vertex is interpreted as being in the same tetrahedron as the domain triangle of the gluing. The output 'Vertex' should be interpreted as being in the same tetrahedron as the codomain triangle of the gluing.
instance GluingMappable Vertex where

    gluingMap gl@(t,ot) v = 
        maybe 
            (error ("gluingMap "++show gl++" "++show v))
            (triangleGetVertexAt (forgetTIndex ot))  
            (triangleGetIndexOf (forgetTIndex t) v)

-- | See comment for the @GluingMappable Vertex@ instance
instance GluingMappable OEdge where
    gluingMap gl (vertices -> (v0,v1)) = oedge (gluingMap gl v0, gluingMap gl v1) 

-- | See comment for the @GluingMappable Vertex@ instance
instance GluingMappable Edge where
    gluingMap gl = forgetVertexOrder . gluingMap gl . toOrderedFace

-- | See comment for the @GluingMappable Vertex@ instance
instance GluingMappable NormalCorner where
    gluingMap gl = normalCorner . gluingMap gl . edge

-- | See comment for the @GluingMappable Vertex@ instance
instance GluingMappable ONormalArc where
    gluingMap gl (normalCorners -> (v0,v1)) = oNormalArc (gluingMap gl v0, gluingMap gl v1) 

gluingMapI
  :: (HasTIndex a a1, GluingMappable a1) =>
     Gluing -> a -> a
gluingMapI gl (viewI -> I i oe) =

        assert (i==glDomTet gl) $ 

        glCodTet gl ./ gluingMap gl oe

instance GluingMappable OIEdge where
    gluingMap = gluingMapI  

instance GluingMappable OINormalArc where
    gluingMap = gluingMapI 

instance GluingMappable IEdge where
    gluingMap = gluingMapI

instance GluingMappable INormalCorner where
    gluingMap = gluingMapI

instance GluingMappable INormalArc where
    gluingMap gl@(tri,otri) arc =

        assert (isSubface arc tri) $
            iNormalArc . (otri,) . gluingMap gl . iNormalArcGetVertex $ arc

instance GluingMappable OITriangle where
    gluingMap (tri,otri) (unpackOrderedFace -> (tri',g)) =
        assert (tri == tri')
            (otri *. g)



inducedVertexEquivalences :: Gluing -> [(IVertex, IVertex)]
inducedVertexEquivalences ((two,otwo') :: Gluing) = 
                zip (vertexList two) (vertexList otwo')


inducedEdgeEquivalences :: Gluing -> [(IEdge, IEdge)]
inducedEdgeEquivalences ((two,otwo') :: Gluing) =
                zip (edgeList two) (forgetVertexOrder <$> edgeList otwo')


inducedOEdgeEquivalencesIrredundant :: Gluing -> [(OIEdge, OIEdge)]
inducedOEdgeEquivalencesIrredundant ((two,otwo') :: Gluing) =
                        zip (edgeList (packOrderedFace two S3abc))
                                       (edgeList otwo')

                                    :: [(OIEdge, OIEdge)]


inducedOEdgeEquivalences :: Gluing -> [(OIEdge, OIEdge)]
inducedOEdgeEquivalences g = do
    pair0 <- inducedOEdgeEquivalencesIrredundant g 
    [ pair0,  map2 (*. Flip) pair0 ]


-- | Is the first triangle smaller than the second?
isGluingNormalized
  :: Gluing -> Bool
isGluingNormalized (tri,otri) = 
    case compare tri (forgetVertexOrder otri) of
         EQ -> $(err') ("isGluingNormalized: Invalid gluing "
                        ++ $(showExps ['tri,'otri]))
         LT -> True
         GT -> False

-- | This function also returns whether the given gluing was already normalized.
normalizeGluing' :: Gluing -> (Bool, NormalizedGluing)
normalizeGluing' gl = second (uncurry UnsafeNormalizedGluing)
    (if isGluingNormalized gl then (True,gl) else (False,flipGluing gl))

normalizeGluing :: Gluing -> NormalizedGluing
normalizeGluing = snd . normalizeGluing'

gluingGen :: Gen Gluing
gluingGen = arbitrary `suchThat` \gl -> fst gl /= forgetVertexOrder (snd gl)




-- | INVARIANT: @ngDom < 'forgetVertexOrder' ngCod@
data NormalizedGluing = UnsafeNormalizedGluing {
    ngDom :: ITriangle,
    ngCod :: OITriangle
}
    deriving (Eq,Ord,Show)

instance Pretty NormalizedGluing where
    prettyPrec = prettyPrecFromShow

ngToGluing :: NormalizedGluing -> Gluing
ngToGluing ng = (ngDom ng, ngCod ng)

ngMap :: GluingMappable a => NormalizedGluing -> a -> a
ngMap = gluingMap . ngToGluing 

gluingMapSum
  :: (SuperSumTy r,
      SubSumTy ab,
      GluingMappable (L ab),
      GluingMappable (R ab),
      R r ~ R ab,
      L r ~ L ab) =>
     Gluing -> ab -> r
gluingMapSum gl = gluingMap gl ++++ gluingMap gl

instance (GluingMappable a, GluingMappable b) => GluingMappable (Either a b) where
    gluingMap = gluingMapSum

deriving instance (GluingMappable a, GluingMappable b) => GluingMappable (DJSimp dim a b) 

--deriving instance (GluingMappable (OTuple' v n)) => GluingMappable (OTuple v n)

instance (GluingMappable v) => GluingMappable (SimplicialTriangleLabelAssoc v) where
    gluingMap gl stla =
        stla {
            stla_left = f (stla_left stla),
            stla_right = f (stla_right stla),
            stla_top = f (stla_top stla)
        }
        where f = gluingMap gl



glDom :: Gluing -> ITriangle
glDom = fst

glCod :: Gluing -> OITriangle
glCod = snd

glDomTet :: Gluing -> TIndex
glDomTet = getTIndex . glDom

glCodTet :: Gluing -> TIndex
glCodTet = getTIndex . glCod

ngDomTet :: NormalizedGluing -> TIndex
ngDomTet = getTIndex . ngDom

ngCodTet :: NormalizedGluing -> TIndex
ngCodTet = getTIndex . ngCod

symmetrizeGluings :: [Gluing] -> [Gluing]
symmetrizeGluings gls = gls ++ map flipGluing gls

instance ShortShow NormalizedGluing where
    shortShow = shortShow . ngDom

glDomOrCodIs :: ITriangle -> Gluing -> Bool
glDomOrCodIs t gl = glDom gl == t || forgetVertexOrder (glCod gl) == t 

ngDomOrCodIs :: ITriangle -> NormalizedGluing -> Bool
ngDomOrCodIs t = glDomOrCodIs t . ngToGluing
