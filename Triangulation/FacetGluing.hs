{-# LANGUAGE StandaloneDeriving, GeneralizedNewtypeDeriving, FlexibleContexts, TemplateHaskell, ScopedTypeVariables, MultiParamTypeClasses, FunctionalDependencies, TupleSections, NoMonomorphismRestriction, TypeFamilies, FlexibleInstances, ViewPatterns #-} 
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS -Wall #-}
module Triangulation.FacetGluing where

import Tetrahedron
import Control.Applicative
import HomogenousTuples
import Data.Maybe
import INormalDisc
import Control.Exception
import Test.QuickCheck
import THUtil
import PrettyUtil
import Either1
import PreRenderable.TriangleLabel
import Control.Arrow

type Gluing = (ITriangle,OITriangle)

gluing :: ITriangle -> OITriangle -> Gluing
gluing = (,)

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

    gluingMap (t,ot) v = 
        maybe (error ("gluingMap: vertex not in left triangle")) 
            (triangleGetVertexAt ot)  
            (triangleGetIndexOf t v)

-- | The input 'Vertex is interpreted as being in the same tetrahedron as the domain triangle of the gluing. The output 'Vertex' should be interpreted as being in the same tetrahedron as the codomain triangle of the gluing.
instance GluingMappable Vertex where

    gluingMap (t,ot) v = 
        maybe (error ("gluingMap: vertex not in left triangle")) 
            (triangleGetVertexAt (forgetTIndex ot))  
            (triangleGetIndexOf (forgetTIndex t) v)

-- | See comment for the @GluingMappable Vertex@ instance
instance GluingMappable OEdge where
    gluingMap gl (vertices -> (v0,v1)) = oedge (gluingMap gl v0, gluingMap gl v1) 

-- | See comment for the @GluingMappable Vertex@ instance
instance GluingMappable Edge where
    gluingMap gl = forgetVertexOrder . gluingMap gl . toOrderedFace

instance GluingMappable OIEdge where
    gluingMap gl@(t,ot) (viewI -> I i oe) =

        assert (i==getTIndex t) $ 

        getTIndex ot ./ gluingMap gl oe 


instance GluingMappable IEdge where
    gluingMap gl = forgetVertexOrder . gluingMap gl . toOrderedFace

instance GluingMappable INormalCorner where
    gluingMap gl = iNormalCorner . gluingMap gl . iNormalCornerGetContainingEdge

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
         EQ -> error ("isGluingNormalized: Invalid gluing "
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

ngToGluing :: NormalizedGluing -> (ITriangle, OITriangle)
ngToGluing ng = (ngDom ng, ngCod ng)

ngMap :: GluingMappable a => NormalizedGluing -> a -> a
ngMap = gluingMap . ngToGluing 

instance (GluingMappable (a n), GluingMappable (b n)) => GluingMappable (Either1 a b n) where
    gluingMap = bimap1 <$> gluingMap <*> gluingMap


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
